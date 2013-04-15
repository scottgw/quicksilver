#ifndef _SYNC_OPS_H
#define _SYNC_OPS_H

#include <stdint.h>

#include "types.h"

sync_data_t
sync_data_new(lfds611_atom_t max_tasks);

void
sync_data_use(sync_data_t);

void
sync_data_enqueue_runnable(sync_data_t sync_data, processor_t proc);

processor_t
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
