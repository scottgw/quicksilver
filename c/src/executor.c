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

bool
exec_steal (executor_t victim_exec, processor_t *proc)
{
  return ws_deque_steal(victim_exec->local_deque, (void**)proc);
}

void
exec_push (executor_t exec, processor_t proc)
{
  ws_deque_push_bottom(exec->local_deque, proc);
  if (ws_deque_size(exec->local_deque) > 4)
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
get_work (executor_t exec)
{
  processor_t proc;
  GArray *executors = sync_data_executors (exec->task->sync_data);
  int len = executors->len;
  int attempts;
  executor_t victim;

  if (!exec_steal(exec, &proc))
    {
      while (true)
        {
          bool steal_success = false;
          
          int vi = 0; // g_random_int_range (0, len);    

          attempts = 0;
          do {
            attempts++;
        
            // Get random victim that's not ourselves
            victim = g_array_index(executors, executor_t, vi);

            if (victim == exec)
              {                
                vi = (vi + 1) % len;
                victim = g_array_index(executors, executor_t, vi);
              }
            vi = (vi + 1) % len;
            steal_success = exec_steal(victim, &proc);
            if (!steal_success)
              {
                steal_success = sync_data_try_dequeue_runnable(exec->task->sync_data, exec, &proc);
              }
          } while (!steal_success && attempts < 4);

          if (steal_success)
            {
              return proc;
            }
          if (!sync_data_wait_for_work (exec->task->sync_data))
            {
              return NULL;
            }
        }
    }
  
  return proc;
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

      assert(proc->task->state >= TASK_TRANSITION_TO_WAITING);
      // If the came back finished, then remove it from the
      // work list.
      switch (proc->task->state)
        {
        case TASK_TRANSITION_TO_RUNNABLE:
          proc->task->state = TASK_RUNNABLE;
          DEBUG_LOG(2, "%p is descheduled by executor %p\n", proc, exec);
          exec_push(exec, proc);
          break;
        case TASK_TRANSITION_TO_WAITING:
          DEBUG_LOG(2, "%p is set to wait by executor %p\n", proc, exec);
          proc->task->state = TASK_WAITING;
          break;
        case TASK_TRANSITION_TO_FINISHED:
          DEBUG_LOG(2, "%p is set to finished by executor %p\n", proc, exec);
          proc->task->state = TASK_FINISHED;
          proc_free(proc);
          break;
        default:
          break;
        }
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
  for(int i = 0; i < n; i++)
    {
      executor_t exec = make_executor(sync_data);
      
      g_array_append_val (executors, exec);
      pthread_create(&exec->thread, NULL, executor_run, exec);
    }
}
