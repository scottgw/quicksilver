#ifndef __SCHED_TASK_H_
#define __SCHED_TASK_H_

#include "types.h"

struct sched_task
{
  struct sched_task * next; // For intrusive allocation in queues.
  task_t task;
  sync_data_t sync_data;
  executor_t executor;
};

typedef struct sched_task* sched_task_t;

sched_task_t
stask_new(sync_data_t);

void
stask_free(sched_task_t);

void
stask_step_state(sched_task_t, executor_t);

void
stask_step_previous(sched_task_t stask);

void
stask_yield_to_executor(sched_task_t);

void
stask_switch(sched_task_t from, sched_task_t to);

void
stask_wake(sched_task_t stask, executor_t exec);

#endif // __SCHED_TASK_H_
