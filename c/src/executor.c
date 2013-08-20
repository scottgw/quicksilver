#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <pthread.h>

#include "libqs/ws_deque.h"
#include "libqs/debug_log.h"
#include "libqs/executor.h"
#include "libqs/processor.h"
#include "libqs/notifier.h"
#include "libqs/task.h"

#define MAX_ATTEMPTS 64
#define MIN_ATTEMPTS 4

pthread_barrier_t barrier;

bool
exec_steal (executor_t victim_exec, processor_t *proc)
{
  assert(victim_exec != NULL);
  return ws_deque_steal(victim_exec->local_deque, (void**)proc);
}

void
exec_push (executor_t exec, processor_t proc)
{
  ws_deque_push_bottom(exec->local_deque, proc);

  // We only inform other processors we have more than 1 piece of work,
  // because this prevents us from contending with the other executors
  // over the only work we have.
  if (ws_deque_size(exec->local_deque) > 1)
    {
      sync_data_signal_work(exec->task->sync_data);
    }
}

bool
exec_pop (executor_t exec, processor_t *proc)
{
  return ws_deque_pop_bottom(exec->local_deque, (void**)proc);
}

processor_t
exec_get_work(executor_t exec, uint32_t attempts)
{
  GArray *executors = sync_data_executors (exec->task->sync_data);
  processor_t proc;
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

      if (exec_steal(victim, &proc))
        {
	  assert (proc->task->state == TASK_RUNNABLE);
          return proc;
        }
    }

  if (sync_data_try_dequeue_runnable(exec->task->sync_data, exec, &proc))
    {
      assert (proc->task->state == TASK_RUNNABLE);
      return proc;
    }
  else
    {
      return NULL;
    }
}


static
processor_t
get_work (executor_t exec)
{
  processor_t proc;

  if (sync_data_try_dequeue_runnable(exec->task->sync_data, exec, &proc))
    {
      return proc;
    }

  if (exec_steal(exec, &proc))
    {
      return proc;
    }

  while (true)
    {
      proc = exec_get_work(exec, MAX_ATTEMPTS);

      if (proc != NULL)
	{
	  return proc;
            }

      if (!sync_data_wait_for_work (exec->task->sync_data))
	{
	  return NULL;
	}
    }
}

void
exec_step_previous(executor_t exec, processor_t ignore_proc)
{
  processor_t null_proc = NULL;
  processor_t last_proc;
  __atomic_exchange (&exec->current_proc, &null_proc, &last_proc,
		     __ATOMIC_SEQ_CST);

  if (last_proc != ignore_proc && last_proc != NULL)
    {
      /* printf("%p transitioning %p\n", proc, last_proc); */
      proc_step_state(last_proc, exec);
    }
}

void
switch_to_next_processor(executor_t exec)
{
  // take a new piece of work.
  BINARY_LOG(2, SYNCOPS_DEQUEUE_START, exec, NULL);
  processor_t proc = get_work (exec);
  BINARY_LOG(2, SYNCOPS_DEQUEUE_END, exec, NULL);
  if (proc != NULL)
    {
      DEBUG_LOG(2, "%p is dequeued by executor %p\n", proc, exec);
      proc->executor = exec;
      exec->current_proc = proc;

      // If this task is to finish, it should restore this executors context.
      proc->task->next = exec->task;

      exec->task->state = TASK_TRANSITION_TO_RUNNABLE;
      yield_to(exec->task, proc->task);
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
  task_free(exec->task);
  free(exec);
}

static
void
executor_loop(void* data)
{
  executor_t exec = (executor_t) data;
  while(!exec->done)
    {
      switch_to_next_processor(exec);
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

  task_set_func(exec->task, executor_loop, exec);
  task_run(exec->task);
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

  exec->task = task_make(sync_data);
  exec->current_proc = NULL;
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
