#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <glib.h>
#include <sys/time.h>
#include <pthread.h>
#include "libqs/sync_ops.h"
#include "libqs/notifier.h"
#include "internal/ws_deque.h"
#include "internal/debug_log.h"
#include "internal/executor.h"
#include "libqs/sched_task.h"
#include "internal/task.h"
#include "trace.h"

#define MAX_ATTEMPTS 64
#define MIN_ATTEMPTS 4

pthread_barrier_t barrier;

bool
exec_steal (executor_t victim_exec, sched_task_t *stask)
{
  assert(victim_exec != NULL);
  return ws_deque_steal(victim_exec->local_deque, (void**)stask);
}

void
exec_push (executor_t exec, sched_task_t stask)
{
  TRACE(QS_EXEC_PUSH());
  ws_deque_push_bottom(exec->local_deque, stask);

  // We only inform other processors we have more than 1 piece of work,
  // because this prevents us from contending with the other executors
  // over the only work we have.
  if (ws_deque_size(exec->local_deque) > 1)
    {
      TRACE(QS_EXEC_SIGNAL_WORK());
      sync_data_signal_work(exec->stask->sync_data);
    }
}

bool
exec_pop (executor_t exec, sched_task_t *stask)
{
  if (ws_deque_size(exec->local_deque) > 0)
    {
      return ws_deque_pop_bottom(exec->local_deque, (void**)stask);
    }
  else
    {
      return false;
    }
}

sched_task_t
exec_get_work(executor_t exec, uint32_t attempts)
{
  GArray *executors = sync_data_executors (exec->stask->sync_data);
  sched_task_t stask;
  uint32_t len = executors->len;
  executor_t victim;

  for (uint32_t i = 0; i < attempts; i++)
    {
      // Get random victim that's not ourselves
      victim = g_array_index(executors, executor_t, i % len);

      if (victim == exec)
        {
          continue;
        }

      if (exec_steal(victim, &stask))
        {
          assert (stask->task->state == TASK_RUNNABLE);
          return stask;
        }
    }

  if (sync_data_try_dequeue_runnable(exec->stask->sync_data, exec, &stask))
    {
      assert (stask->task->state == TASK_RUNNABLE);
      return stask;
    }
  else
    {
      return NULL;
    }
}


static
bool
get_work (executor_t exec, sched_task_t *result)
{
  sync_data_t sync_data = exec->stask->sync_data;

  if (sync_data_try_dequeue_runnable(sync_data, exec, result))
    {
      assert (*result != NULL);
      return true;
    }

  if (exec_pop(exec, result))
    {
      assert (*result != NULL);
      return true;
    }

  while (true)
    {
      *result = exec_get_work(exec, MAX_ATTEMPTS);

      if (*result != NULL)
        {
          return true;
        }

      sync_data_wait_for_work (sync_data);

      if (sync_data_num_processors(sync_data) == 0)
        {
          assert (sync_data_num_processors(sync_data) == 0);
          return false;
        }
    }
}

void
exec_step_previous(executor_t exec, sched_task_t ignore_stask)
{
  sched_task_t null_stask = NULL;
  sched_task_t last_stask;
  __atomic_exchange (&exec->prev_stask, &null_stask, &last_stask,
                     __ATOMIC_SEQ_CST);

  if (last_stask != ignore_stask && last_stask != NULL)
    {
      /* printf("%p transitioning %p\n", proc, last_proc); */
      stask_step_state(last_stask, exec);
    }
}

void
switch_to_next_task(executor_t exec)
{
  sched_task_t stask = NULL;

  if (get_work (exec, &stask))
    {
      assert (exec != NULL);
      stask->executor = exec;
      exec->prev_stask = NULL;

      // If this task is to finish, it should restore this executors context.
      stask->task->next = exec->stask->task;

      exec->stask->task->state = TASK_TRANSITION_TO_RUNNABLE;
      task_switch(exec->stask->task, stask->task);
      exec_step_previous (exec, NULL);
    }
  else
    {
      exec->done = true;
    }
}

void
executor_free(executor_t exec)
{
  stask_free(exec->stask);
  ws_deque_free(exec->local_deque);
  free(exec);
}

static
void
executor_loop(void* data)
{
  executor_t exec = (executor_t) data;
  while(!exec->done)
    {
      switch_to_next_task(exec);
    }
  notifier_done = 1;
  pthread_exit(NULL);
}

void*
executor_run(executor_t exec)
{
  sync_data_barrier_wait(exec->stask->sync_data);

  task_set_func_and_run(exec->stask->task, executor_loop, exec);

  assert (false && "executor_run: should never reach this point");
  return NULL;
}

int exec_count = 0;

// Constructs the executor thread and adds the executor
// To the list of executors.
executor_t
exec_make(sync_data_t sync_data)
{
  executor_t exec = malloc(sizeof(struct executor));

  // We don't register this task as it doesn't count towards the global
  // count of tasks.
  exec->stask = stask_new_no_register(sync_data);
  exec->prev_stask = NULL;
  exec->done = false;
  exec->id = 0;
  exec->backoff_us = 500;
  exec->local_deque = ws_deque_new ();

  return exec;
}
