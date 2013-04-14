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

void
add_sleeper(sync_data_t sync_data, task_t task, struct timespec duration)
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
get_sleepers(sync_data_t sync_data,
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

