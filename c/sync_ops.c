#include <assert.h>
#include <time.h>

#include "sync_ops.h"
#include "processor.h"
#include "task.h"
#include "liblfds611.h"

// global sync data
struct sync_data
{
  lfds611_atom_t max_tasks;

  volatile uint64_t num_processors;

  conc_list_t sleep_list;

  conc_queue_t runnable_queue;
  volatile uint64_t run_queue_size;
  pthread_mutex_t run_mutex;
  pthread_cond_t not_empty;
};


struct sleeper
{
  processor_t proc;
  struct timespec end_time;
};

typedef struct sleeper* sleeper_t;

static
void
sleep_free(void *item, void *user_data)
{
  free(item);
}

sync_data_t
sync_data_new(lfds611_atom_t max_tasks)
{
  sync_data_t result = (sync_data_t)malloc(sizeof(struct sync_data));

  result->num_processors = 0;
  result->max_tasks = max_tasks;

  assert(lfds611_queue_new(&result->runnable_queue, max_tasks) == 1);
  result->run_queue_size = 0;
  pthread_mutex_init(&result->run_mutex, NULL);
  pthread_cond_init(&result->not_empty, NULL);

  assert(lfds611_slist_new(&result->sleep_list, sleep_free, NULL) == 1);

  return result;
}

void
sync_data_free(sync_data_t sync_data)
{
  pthread_cond_destroy(&sync_data->not_empty);
  pthread_mutex_destroy(&sync_data->run_mutex);
  lfds611_queue_delete(sync_data->runnable_queue, NULL, NULL);
  lfds611_slist_delete(sync_data->sleep_list);
}

void
sync_data_use(sync_data_t sync_data)
{  
  lfds611_queue_use(sync_data->runnable_queue);
  lfds611_slist_use(sync_data->sleep_list);
}


/* -------------------- */
/* Run queue operations */
/* -------------------- */

void
sync_data_enqueue_runnable(sync_data_t sync_data, processor_t proc)
{
  assert(proc != NULL);
  assert(proc->task != NULL);
  assert(proc->task->state == TASK_RUNNABLE);
  assert(lfds611_queue_enqueue(sync_data->runnable_queue, proc) == 1);

  if (__sync_fetch_and_add(&sync_data->run_queue_size, 1) == 0)
    {
      // if this enqueue makes the queue not empty then broadcast
      pthread_cond_broadcast(&sync_data->not_empty);
    }
}

processor_t
sync_data_dequeue_runnable(sync_data_t sync_data)
{
  if (sync_data->num_processors > 0)
    {
      processor_t proc;

      if (lfds611_queue_dequeue(sync_data->runnable_queue, (void**)&proc) == 1)
        {
          __sync_fetch_and_sub(&sync_data->run_queue_size, 1);
          assert(proc->task->state == TASK_RUNNABLE);
        }
      else
        {
          pthread_mutex_lock(&sync_data->run_mutex);

          while (lfds611_queue_dequeue
                 (sync_data->runnable_queue, (void**)&proc) != 1 && 
                 sync_data->num_processors > 0)
            {
              pthread_cond_wait(&sync_data->not_empty, &sync_data->run_mutex);
            }

          __sync_fetch_and_sub(&sync_data->run_queue_size, 1);
          pthread_mutex_unlock(&sync_data->run_mutex);
        }
      if (sync_data->num_processors > 0)
        {
          assert (proc != NULL);
          return proc;
        }
      else
        {
          pthread_cond_broadcast(&sync_data->not_empty);
          printf("sync_data_dequeue: no more processors\n");
          return NULL;
        }
    }
  else
    {
      pthread_cond_broadcast(&sync_data->not_empty);
      printf("sync_data_dequeue: no more processors\n");
      return NULL;
    }
}


/* ---------------------- */
/* Processor registration */
/* ---------------------- */
void
sync_data_register_proc(sync_data_t sync_data)
{
  __sync_fetch_and_add(&sync_data->num_processors, 1);
}

void
sync_data_deregister_proc(sync_data_t sync_data)
{
  if (__sync_sub_and_fetch(&sync_data->num_processors, 1) == 0)
    {
      pthread_cond_broadcast(&sync_data->not_empty);
    }
}

uint64_t
sync_data_num_processors(sync_data_t sync_data)
{
  return sync_data->num_processors;
}


/* ---------------- */
/* Sleep operations */
/* ---------------- */

void
sync_data_add_sleeper(sync_data_t sync_data,
                      processor_t proc,
                      struct timespec duration)
{
  assert (proc != NULL);
  assert (proc->task != NULL);
  assert (proc->task->state == TASK_RUNNING);

  conc_list_elem_t item;
  conc_list_t sleepers = sync_data->sleep_list;
  sleeper_t sleeper = (sleeper_t) malloc(sizeof(struct sleeper));

  struct timespec current_time;
  clock_gettime(CLOCK_REALTIME, &current_time);

  proc->task->state = TASK_WAITING;
  sleeper->proc = proc;
  sleeper->end_time.tv_sec = current_time.tv_sec + duration.tv_sec;
  sleeper->end_time.tv_nsec = current_time.tv_nsec + duration.tv_nsec;

  item = lfds611_slist_new_head (sleepers, sleeper);

  assert (item != NULL && "Insertion failed");
}

void
sync_data_get_sleepers(sync_data_t sync_data,
                       processor_t **procs,
                       uint64_t *num_awoken)
{
  conc_list_t sleepers = sync_data->sleep_list;

  struct timespec current_time;
  clock_gettime(CLOCK_REALTIME, &current_time);
  
  conc_list_elem_t item = NULL;

  // Count the number of tasks to wake up.
  uint64_t n = 0;
  while( lfds611_slist_get_head_and_then_next(sleepers, &item))
    {
      sleeper_t sleeper;
      lfds611_slist_get_user_data_from_element(item, (void**)&sleeper);
      if (sleeper->end_time.tv_sec >= current_time.tv_sec &&
          sleeper->end_time.tv_nsec >= current_time.tv_nsec)
        {
          n++;
        }
    }

  *procs = (processor_t*) malloc(sizeof(struct processor)*n);

  n = 0;
  // Put the tasks in an array.
  while( lfds611_slist_get_head_and_then_next(sleepers, &item))
    {
      sleeper_t sleeper;
      lfds611_slist_get_user_data_from_element(item, (void**)&sleeper);
      if (sleeper->end_time.tv_sec >= current_time.tv_sec &&
          sleeper->end_time.tv_nsec >= current_time.tv_nsec)
        {
          assert (sleeper->proc != NULL);
          (*procs)[n] = sleeper->proc;
          sleeper->proc->task->state = TASK_RUNNABLE;
          n++;
          lfds611_slist_logically_delete_element(sleepers, item);
        }
    }

  *num_awoken = n;
}

