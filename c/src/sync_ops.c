#include <assert.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

#include "libqs/debug_log.h"
#include "libqs/sync_ops.h"
#include "libqs/processor.h"
#include "libqs/task.h"
#include "libqs/queue_impl.h"

// global sync data
struct sync_data
{
  uint32_t max_tasks;

  volatile uint64_t num_processors;

  queue_impl_t sleep_list;
  volatile uint64_t num_sleepers;

  queue_impl_t runnable_queue;
  volatile uint64_t num_runnable;
  pthread_mutex_t run_mutex;
  pthread_cond_t not_empty;
};

sync_data_t
sync_data_new(uint32_t max_tasks)
{
  sync_data_t sync_data = (sync_data_t)malloc(sizeof(struct sync_data));

  char* libqs_log_str = getenv("LIBQS_LOG");
  if (libqs_log_str == NULL)
    {
      log_setup(0);
    }
  else
    {
      log_setup(atoi(libqs_log_str));
    }

  sync_data->num_processors = 0;
  sync_data->max_tasks = max_tasks;

  sync_data->runnable_queue = queue_impl_new(max_tasks);
  sync_data->num_runnable = 0;
  pthread_mutex_init(&sync_data->run_mutex, NULL);
  pthread_cond_init(&sync_data->not_empty, NULL);

  sync_data->num_sleepers = 0;

  sync_data->sleep_list = queue_impl_new(max_tasks);

  return sync_data;
}

void
sync_data_free(sync_data_t sync_data)
{
  pthread_cond_destroy(&sync_data->not_empty);
  pthread_mutex_destroy(&sync_data->run_mutex);
  queue_impl_free(sync_data->runnable_queue);
  queue_impl_free(sync_data->sleep_list);
  free(sync_data);
  log_write();
}

void
sync_data_use(sync_data_t sync_data)
{  
  queue_impl_use(sync_data->runnable_queue);
  queue_impl_use(sync_data->sleep_list);
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
  
  bool success = queue_impl_enqueue(sync_data->runnable_queue, proc);
  assert(success);

  /* __sync_fetch_and_add(&sync_data->num_runnable, 1); */

  pthread_mutex_lock(&sync_data->run_mutex);
  pthread_cond_signal(&sync_data->not_empty);
  pthread_mutex_unlock(&sync_data->run_mutex);
}

processor_t
sync_data_dequeue_runnable(sync_data_t sync_data, void* exec)
{
  volatile processor_t proc;
  proc = NULL;

  for (int i = 0; i < 1024; i++)
    {
      if (queue_impl_dequeue(sync_data->runnable_queue, (void**)&proc))
        {
          return proc;
        }
    }

  if (!queue_impl_dequeue(sync_data->runnable_queue, (void**)&proc))
    {
      usleep(500);
      logs(1, "%p runnable dequeue start\n", exec);
      pthread_mutex_lock(&sync_data->run_mutex);
      while (!queue_impl_dequeue (sync_data->runnable_queue, (void**)&proc) && 
             sync_data->num_processors > 0)
        {
          pthread_cond_wait(&sync_data->not_empty, &sync_data->run_mutex);
        }
      pthread_mutex_unlock(&sync_data->run_mutex);
      logs(1, "%p runnable dequeue end\n", exec);
    }

  if (sync_data->num_processors > 0 &&
      proc != NULL)
    {
      assert(proc->task->state == TASK_RUNNABLE);
      /* __sync_fetch_and_sub(&sync_data->num_runnable, 1); */
    }
  else
    {
      proc = NULL;
      pthread_cond_broadcast(&sync_data->not_empty);    
    }

  return proc;
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

  queue_impl_t sleepers = sync_data->sleep_list;
  sleeper_t sleeper = (sleeper_t) malloc(sizeof(struct sleeper));

  __sync_fetch_and_add(&sync_data->num_sleepers, 1);

  struct timespec current_time;
  clock_gettime(CLOCK_REALTIME, &current_time);

  proc->task->state = TASK_WAITING;
  sleeper->proc = proc;
  sleeper->end_time.tv_sec = current_time.tv_sec + duration.tv_sec;
  sleeper->end_time.tv_nsec = current_time.tv_nsec + duration.tv_nsec;

  bool success = queue_impl_enqueue(sleepers, sleeper);

  assert (success && "Insertion failed");
}

static
bool
keep_sleeping(void *elem, void *user)
{
  sleeper_t sleeper = (sleeper_t) elem;
  struct timespec *current_time = (struct timespec*) user;
  
  return
    sleeper->end_time.tv_sec >= current_time->tv_sec ||
    (sleeper->end_time.tv_sec == current_time->tv_sec && 
     sleeper->end_time.tv_nsec >= current_time->tv_nsec);
}

queue_impl_t
sync_data_get_sleepers(sync_data_t sync_data)
{
  queue_impl_t sleepers = sync_data->sleep_list;

  struct timespec current_time;
  clock_gettime(CLOCK_REALTIME, &current_time);

  queue_impl_t awakened = 
    queue_impl_filter_out(sleepers, keep_sleeping, &current_time);

  int n = queue_impl_size(awakened);

  __sync_fetch_and_sub(&sync_data->num_sleepers, n);
  return awakened;
}
