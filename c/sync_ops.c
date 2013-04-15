#include <assert.h>
#include <time.h>

#include "sync_ops.h"
#include "liblfds611.h"

struct sleeper
{
  task_t task;
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
sync_data_new(uint64_t max_tasks)
{
  sync_data_t result = (sync_data_t)malloc(sizeof(struct sync_data));

  result->max_tasks = max_tasks;

  assert(lfds611_queue_new(&result->runnable_queue, max_tasks) == 1);
  assert(lfds611_slist_new(&result->sleep_list, sleep_free, NULL) == 1);

  return result;
}

void
sync_data_enqueue_runnable(sync_data_t sync_data, task_t task)
{
  assert(task != NULL);
  assert(task->state != TASK_RUNNING);
  assert(lfds611_queue_enqueue(sync_data->runnable_queue, task) == 1);
}

task_t
sync_data_dequeue_runnable(sync_data_t sync_data)
{
  task_t task = NULL;

  if (lfds611_queue_dequeue(sync_data->runnable_queue, (void**)&task) == 1)
    {
      assert(task->state == TASK_RUNNABLE);
    }

  return task;  
}


void
sync_data_add_sleeper(sync_data_t sync_data, task_t task, struct timespec duration)
{
  conc_list_t sleepers = sync_data->sleep_list;
  sleeper_t sleeper = (sleeper_t) malloc(sizeof(struct sleeper));

  struct timespec current_time;
  clock_gettime(CLOCK_REALTIME, &current_time);

  sleeper->task = task;
  sleeper->end_time.tv_sec = current_time.tv_sec + duration.tv_sec;
  sleeper->end_time.tv_nsec = current_time.tv_nsec + duration.tv_nsec;

  conc_list_elem_t item = lfds611_slist_new_head (sleepers, sleeper);
  assert (item != NULL && "Insertion failed");
}

void
sync_data_get_sleepers(sync_data_t sync_data,
                       task_t **tasks,
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

  *tasks = (task_t*) malloc(sizeof(struct task)*n);

  n = 0;
  // Put the tasks in an array.
  while( lfds611_slist_get_head_and_then_next(sleepers, &item))
    {
      sleeper_t sleeper;
      lfds611_slist_get_user_data_from_element(item, (void**)&sleeper);
      if (sleeper->end_time.tv_sec >= current_time.tv_sec &&
          sleeper->end_time.tv_nsec >= current_time.tv_nsec)
        {
          lfds611_slist_logically_delete_element(sleepers, item);
          (*tasks)[n] = sleeper->task;
          free(sleeper);
          n++;
        }
    }

  *num_awoken = n;
}

