#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "libqs/sync_ops.h"
#include "libqs/sched_task.h"
#include "internal/executor.h"
#include "internal/task.h"
#include "trace.h"
static
sched_task_t
stask_new_register(sync_data_t sync_data, bool register_task)
{
  sched_task_t stask = malloc(sizeof(struct sched_task));
  stask_init(stask, sync_data, register_task);
  return stask;
}

void
stask_init(sched_task_t stask, sync_data_t sync_data, bool register_task)
{
  stask->task = task_make();
  stask->registr = register_task;
  stask->sync_data = sync_data;
  stask->executor = (void*)0xDEADBEEF;
  stask->next = NULL;

  if (register_task)
    {
      sync_data_register_task(sync_data);
    }  
}

sched_task_t
stask_new(sync_data_t sync_data)
{
  return stask_new_register(sync_data, true);
}

sched_task_t
stask_new_no_register(sync_data_t sync_data)
{
  return stask_new_register(sync_data, false);
}

void
stask_free(sched_task_t stask)
{
  if (stask->registr)
    {
      sync_data_deregister_task(stask->sync_data);
    }
  task_free(stask->task);
  free(stask);
}

typedef struct {
  void (*func)(void*);
  void *data;
  sched_task_t stask;
} stask_data;


void
stask_wrapper(void* data)
{
  stask_data *sdata = (stask_data*)data;
  void (*func)(void*) = sdata->func;
  void *real_data = sdata->data;
  sched_task_t stask = sdata->stask;

  stask_step_previous(stask);
  func(real_data);
  stask->executor->prev_stask = stask;

  free(sdata);
}

/*!
  Set the sched_task to run a particular function.
*/
void
stask_set_func(sched_task_t stask, void (*f)(void*), void* data)
{
  stask_data* sdata = malloc(sizeof(stask_data));
  sdata->func = f;
  sdata->data = data;
  sdata->stask = stask;
  task_set_func(stask->task, stask_wrapper, sdata);
}

/*!
  Look at the previous processor and runs proc_step_state on it
  if it is not the argument processor.

  \param proc processor whose executor will be checked for the
  previous processor.
*/
void
stask_step_previous(sched_task_t stask)
{
  executor_t exec = stask->executor;
  exec_step_previous (exec, stask);
}

void
stask_step_state(sched_task_t stask, executor_t exec)
{
  assert(stask != NULL);
  assert(stask->task->state >= TASK_TRANSITION_TO_WAITING);

  switch (stask->task->state)
    {
    case TASK_TRANSITION_TO_RUNNABLE:
      stask->task->state = TASK_RUNNABLE;
      exec_push(exec, stask);
      break;
    case TASK_TRANSITION_TO_WAITING:
      /* printf("%p set to waiting\n", stask); */
      stask->task->state = TASK_WAITING;
      break;
    case TASK_TRANSITION_TO_FINISHED:
      stask->task->state = TASK_FINISHED;
      // assert("How to free the processor? cast up?" && 0);
      stask_free(stask);
      break;
    default:
      break;
    }
}


void
stask_switch(sched_task_t from, sched_task_t to)
{
  assert (from != to);
  executor_t exec = from->executor;
  exec->prev_stask = from;

  if (to == NULL)
    {
      task_switch(from->task, exec->stask->task);
    }
  else
    {
      assert (exec != NULL);
      to->executor = exec;

      /* // If this task is to finish, it should come back to this processor. */
      /* // This decision may be dubious, perhaps it should switch to the */
      /* // executor? */
      /* if (proc->task->state == TASK_TRANSITION_TO_RUNNABLE) */
      /*   { */
      /*     next_proc->task->next = proc->task; */
      /*   } */
      /* else */
      /*   { */
      /*     next_proc->task->next = exec->task; */
      /*   } */

      to->task->next = exec->stask->task;

      /* printf("%p directly yielding to %p\n", from, to); */
      // Switch to the task we found.
      task_switch(from->task, to->task);
    }

  stask_step_previous(from);
}

void
stask_yield_to_executor(sched_task_t stask)
{
  assert (stask->task->state >= TASK_TRANSITION_TO_WAITING);
  executor_t exec = stask->executor;
  bool found;
  sched_task_t next_stask;
  TRACE(QS_STASK_FIND_NEXT_START());
  if (sync_data_try_dequeue_runnable (stask->sync_data, exec, &next_stask))
    {
      goto yield;
    }

  TRACE(QS_STASK_FIND_NEXT_DEQUEUE_END());

  found = exec_pop(exec, &next_stask);

  TRACE(QS_STASK_FIND_NEXT_POP_END());

  if (found)
    {
      TRACE(QS_STASK_POP_SUCCESS());
      goto yield;
      /* printf("%p stole %p\n", proc, next_proc); */
    }

  next_stask = exec_get_work(exec, 8);

  TRACE(QS_STASK_FIND_NEXT_GET_WORK_END());

 yield:
  TRACE(QS_STASK_FIND_NEXT_END());
  stask_switch(stask, next_stask);
}

void
stask_wake(sched_task_t stask, executor_t exec)
{ 
  assert(exec != NULL);
  assert(stask != NULL);
  while(task_get_state(stask->task) != TASK_WAITING);
  task_set_state(stask->task, TASK_RUNNABLE);
  exec_push(exec, stask);
  /* sync_data_enqueue_runnable(proc->task->sync_data, proc); */
}
