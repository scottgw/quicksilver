#ifndef _SYNC_OPS_H
#define _SYNC_OPS_H

#include <stdint.h>
#include "liblfds611.h"

#include "task.h"

typedef struct lfds611_slist_state* conc_list_t;
typedef struct lfds611_slist_element* conc_list_elem_t;

struct sync_data 
{
  conc_list_t sleep_list;
};

typedef struct sync_data* sync_data_t;

void
add_sleeper(sync_data_t sync_data, task_t task, struct timespec duration);

void
get_sleepers(sync_data_t sync_data,
             task_t **tasks,
             uint64_t *num_awoken);


#endif // _SYNC_OPS_H
