#ifndef __SCHED_TASK_H_
#define __SCHED_TASK_H_

#include "../libqs/types.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
  IO_SATISFIED,
  IO_UNSATISFIED,
  IO_STATUS_UNKNOWN
} io_status_t;

struct sched_task
{
  struct sched_task * volatile next; // For intrusive allocation in queues.
  volatile io_status_t io_status;
  task_t task;
  bool registr;
  sync_data_t sync_data;
  executor_t executor;
};

sched_task_t
stask_new(sync_data_t);

sched_task_t
stask_new_no_register(sync_data_t sync_data);

void
stask_init(sched_task_t stask, sync_data_t sync_data, bool register_task);

void
stask_free(sched_task_t);

/*!
  Set the sched_task to run a particular function.
*/
void
stask_set_func(sched_task_t stask, void (*f)(void*), void* data);

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
