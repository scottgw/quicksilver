#ifndef _SYNC_OPS_H
#define _SYNC_OPS_H

#include <stdint.h>

#include "queue_impl.h"
#include "types.h"

struct sleeper
{
  processor_t proc;
  struct timespec end_time;
};

typedef struct sleeper* sleeper_t;


sync_data_t
sync_data_new(uint32_t max_tasks);

void
sync_data_free(sync_data_t sync_data);

void
sync_data_use(sync_data_t);

void
sync_data_register_proc(sync_data_t sync_data);

void
sync_data_deregister_proc(sync_data_t sync_data);

uint64_t
sync_data_num_processors(sync_data_t sync_data);

void
sync_data_enqueue_runnable(sync_data_t sync_data, processor_t proc);

processor_t
sync_data_dequeue_runnable(sync_data_t sync_data, void* exec);

void
sync_data_add_sleeper(sync_data_t sync_data,
                      processor_t proc,
                      struct timespec duration);

queue_impl_t
sync_data_get_sleepers(sync_data_t sync_data);


#endif // _SYNC_OPS_H
