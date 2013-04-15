#ifndef _SYNC_OPS_H
#define _SYNC_OPS_H

#include <stdint.h>
#include "liblfds611.h"

#include "task.h"

typedef struct lfds611_queue_state* conc_queue_t;

typedef struct lfds611_slist_state* conc_list_t;
typedef struct lfds611_slist_element* conc_list_elem_t;

struct sync_data 
{
  uint64_t max_tasks;
  conc_list_t sleep_list;
  conc_queue_t runnable_queue;
};

typedef struct sync_data* sync_data_t;


sync_data_t
sync_data_new(uint64_t max_tasks);

void
sync_data_enqueue_runnable(sync_data_t sync_data, task_t task);

task_t
sync_data_dequeue_runnable(sync_data_t sync_data);

void
sync_data_add_sleeper(sync_data_t sync_data,
                      task_t task,
                      struct timespec duration);

void
sync_data_get_sleepers(sync_data_t sync_data,
             task_t **tasks,
             uint64_t *num_awoken);


#endif // _SYNC_OPS_H
