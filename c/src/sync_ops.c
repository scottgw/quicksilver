#include <assert.h>
#include <stdlib.h>
#include <pthread.h>
#include <time.h>
#include <unistd.h>

#include "libqs/sync_ops.h"

#include "internal/sched_task.h"
#include "internal/executor.h"
#include "internal/task.h"
#include "internal/queue_impl.h"
#include "internal/debug_log.h"


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

  // List of executors
  GArray *executors;
  // Barrier for executors
  pthread_barrier_t barrier;
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

  sync_data->executors = g_array_new (false, false, sizeof(executor_t));

  return sync_data;
}

static
void*
run_executor(void* data)
{
  return executor_run((executor_t)data);
}

static
void
join_executor(executor_t exec)
{
  pthread_join(exec->thread, NULL);
  executor_free(exec);
}

void
sync_data_barrier_wait(sync_data_t sync_data)
{
  pthread_barrier_wait(&sync_data->barrier);
}

void
sync_data_create_executors(sync_data_t sync_data, uint32_t n)
{
  GArray *executors = sync_data->executors;
  pthread_barrier_init(&sync_data->barrier, NULL, n);
  for(int i = 0; i < n; i++)
    {
      executor_t exec = exec_make(sync_data);
      
      g_array_append_val (executors, exec);
      pthread_create(&exec->thread, NULL, run_executor, exec);
    }
}

void
sync_data_join_executors(sync_data_t sync_data)
{
  GArray *executors = sync_data->executors;
  for (int i = 0; i < executors->len; i++)
    {
      join_executor (g_array_index (executors, executor_t, i));
    }
}


void
sync_data_free(sync_data_t sync_data)
{
  pthread_cond_destroy(&sync_data->not_empty);
  pthread_mutex_destroy(&sync_data->run_mutex);
  queue_impl_free(sync_data->runnable_queue);
  queue_impl_free(sync_data->sleep_list);
  free(sync_data);
  binary_write();
}

GArray*
sync_data_executors(sync_data_t sync_data)
{
  return sync_data->executors;
}


void
sync_data_signal_work(sync_data_t sync_data)
{
  pthread_cond_signal(&sync_data->not_empty);
}


bool
sync_data_wait_for_work(sync_data_t sync_data)
{
  struct timespec current_time;
  clock_gettime(CLOCK_REALTIME, &current_time);
  current_time.tv_nsec += 10*1000*1000;

  pthread_mutex_lock(&sync_data->run_mutex);
  pthread_cond_timedwait(&sync_data->not_empty,
                         &sync_data->run_mutex,
                         &current_time);
  pthread_mutex_unlock(&sync_data->run_mutex);

  return sync_data->num_processors > 0;
}

/* -------------------- */
/* Run queue operations */
/* -------------------- */

void
sync_data_enqueue_runnable(sync_data_t sync_data, sched_task_t stask)
{
  assert(stask != NULL);
  assert(stask->task != NULL);
  assert(stask->task->state == TASK_RUNNABLE);
  
  bool success = queue_impl_enqueue(sync_data->runnable_queue, stask);
  assert(success);

  /* __sync_fetch_and_add(&sync_data->num_runnable, 1); */

  pthread_mutex_lock(&sync_data->run_mutex);
  pthread_cond_signal(&sync_data->not_empty);
  pthread_mutex_unlock(&sync_data->run_mutex);
}

sched_task_t
sync_data_dequeue_runnable(sync_data_t sync_data, executor_t exec)
{
  volatile sched_task_t stask;
  stask = NULL;

  for (int i = 0; i < 1024; i++)
    {
      if (queue_impl_dequeue(sync_data->runnable_queue, (void**)&stask))
        {
          return stask;
        }
    }

  if (!queue_impl_dequeue(sync_data->runnable_queue, (void**)&stask))
    {
      DEBUG_LOG(1, "%p runnable dequeue start\n", exec);
      pthread_mutex_lock(&sync_data->run_mutex);
      while (!queue_impl_dequeue (sync_data->runnable_queue, (void**)&stask) && 
             sync_data->num_processors > 0)
        {
          pthread_cond_wait(&sync_data->not_empty, &sync_data->run_mutex);
        }
      pthread_mutex_unlock(&sync_data->run_mutex);
      DEBUG_LOG(1, "%p runnable dequeue end\n", exec);
    }

  if (sync_data->num_processors > 0 &&
      stask != NULL)
    {
      assert(stask->task->state == TASK_RUNNABLE);
      /* __sync_fetch_and_sub(&sync_data->num_runnable, 1); */
    }
  else
    {
      stask = NULL;
      pthread_cond_broadcast(&sync_data->not_empty);    
    }
  return stask;
}

bool
sync_data_try_dequeue_runnable(sync_data_t sync_data, executor_t exec, sched_task_t *stask)
{
  return queue_impl_dequeue(sync_data->runnable_queue, (void**) stask);
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
                      sched_task_t stask,
                      struct timespec duration)
{
  assert (stask != NULL);
  assert (stask->task != NULL);
  assert (stask->task->state == TASK_RUNNING);

  queue_impl_t sleepers = sync_data->sleep_list;
  sleeper_t sleeper = (sleeper_t) malloc(sizeof(struct sleeper));

  __sync_fetch_and_add(&sync_data->num_sleepers, 1);

  struct timespec current_time;
  clock_gettime(CLOCK_REALTIME, &current_time);

  stask->task->state = TASK_WAITING;
  sleeper->stask = stask;
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
