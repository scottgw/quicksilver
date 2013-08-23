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
#include "internal/sched_task.h"
#include "internal/task.h"

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
  ws_deque_push_bottom(exec->local_deque, stask);

  // We only inform other processors we have more than 1 piece of work,
  // because this prevents us from contending with the other executors
  // over the only work we have.
  if (ws_deque_size(exec->local_deque) > 1)
    {
      sync_data_signal_work(exec->stask->sync_data);
    }
}

bool
exec_pop (executor_t exec, sched_task_t *stask)
{
  return ws_deque_pop_bottom(exec->local_deque, (void**)stask);
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
sched_task_t
get_work (executor_t exec)
{
  sched_task_t stask;

  if (sync_data_try_dequeue_runnable(exec->stask->sync_data, exec, &stask))
    {
      return stask;
    }

  if (exec_steal(exec, &stask))
    {
      return stask;
    }

  while (true)
    {
      stask = exec_get_work(exec, MAX_ATTEMPTS);

      if (stask != NULL)
	{
	  return stask;
            }

      if (!sync_data_wait_for_work (exec->stask->sync_data))
	{
	  return NULL;
	}
    }
}

void
exec_step_previous(executor_t exec, sched_task_t ignore_stask)
{
  sched_task_t null_stask = NULL;
  sched_task_t last_stask;
  __atomic_exchange (&exec->current_stask, &null_stask, &last_stask,
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
  // take a new piece of work.
  BINARY_LOG(2, SYNCOPS_DEQUEUE_START, exec, NULL);
  sched_task_t stask = get_work (exec);
  BINARY_LOG(2, SYNCOPS_DEQUEUE_END, exec, NULL);
  if (stask != NULL)
    {
      DEBUG_LOG(2, "%p is dequeued by executor %p\n", stask, exec);
      stask->executor = exec;
      exec->current_stask = stask;

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

static 
void*
executor_run(void* data)
{
  executor_t exec = data;

  pthread_barrier_wait(&barrier);

  task_set_func_and_run(exec->stask->task, executor_loop, exec);

  assert (false && "executor_run: should never reach this point");  
  return NULL;
}

static
void
join_executor(executor_t exec)
{
  pthread_join(exec->thread, NULL);
  executor_free(exec);
}

int exec_count = 0;

// Constructs the executor thread and adds the executor
// To the list of executors.
executor_t
make_executor(sync_data_t sync_data)
{
  executor_t exec = malloc(sizeof(struct executor));

  exec->stask = stask_new(sync_data);
  exec->current_stask = NULL;
  exec->done = false;
  exec->id = exec_count++;
  exec->backoff_us = 500;
  exec->local_deque = ws_deque_new ();
  return exec;
}

// Joins the list of executors.
void
join_executors(sync_data_t sync_data)
{
  GArray *executors = sync_data_executors(sync_data);
  for (int i = 0; i < executors->len; i++)
    {
      join_executor (g_array_index (executors, executor_t, i));
    }
}

void
create_executors(sync_data_t sync_data, int n)
{
  GArray *executors = sync_data_executors(sync_data);
  pthread_barrier_init(&barrier, NULL, n);
  for(int i = 0; i < n; i++)
    {
      executor_t exec = make_executor(sync_data);
      
      g_array_append_val (executors, exec);
      pthread_create(&exec->thread, NULL, executor_run, exec);
    }
}
