#ifndef __SCHED_TASK_H_
#define __SCHED_TASK_H_

#include "../libqs/types.h"

#ifdef __cplusplus
extern "C" {
#endif

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

/*!
  Step the state of the task.
  This is used when the task is in a transitional state
  and must be moved to its final state.

  \param stask schedulable task to step
  \param exec executor to push the task into if its runnable
*/
void
stask_step_state(sched_task_t stask, executor_t exec);

void
stask_step_previous(sched_task_t stask);


/*!
  Yield the task to its executor.
  
  \param stask task to yield.
*/
void
stask_yield_to_executor(sched_task_t);

void
stask_switch(sched_task_t from, sched_task_t to);

/*!
  Wake the task up on the given executor.
  The executor should be the one that is currently active,
  this is meant to be run from a current running schedulable task.

  \param stask the schedulable task to wake
  \param exec the executor_t that it will be awoken on.
*/
void
stask_wake(sched_task_t stask, executor_t exec);


#ifdef __cplusplus
}
#endif


#endif // __SCHED_TASK_H_
