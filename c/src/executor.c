#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <pthread.h>

#include "libqs/debug_log.h"
#include "libqs/executor.h"
#include "libqs/processor.h"
#include "libqs/list.h"
#include "libqs/notifier.h"
#include "libqs/task.h"

list_t executors;

static
void
switch_to_next_processor(executor_t exec)
{
  // take a new piece of work.

  volatile processor_t proc = sync_data_dequeue_runnable(exec->task->sync_data, exec);

  if (proc != NULL)
    {
      logs(2, "%p is dequeued by executor %p\n", proc, exec);
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
          logs(2, "%p is descheduled by executor %p\n", proc, exec);
          sync_data_enqueue_runnable(exec->task->sync_data, proc);
          break;
        case TASK_TRANSITION_TO_WAITING:
          logs(2, "%p is set to wait by executor %p\n", proc, exec);
          proc->task->state = TASK_WAITING;
          break;
        case TASK_TRANSITION_TO_FINISHED:
          logs(2, "%p is set to finished by executor %p\n", proc, exec);
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
join_executor(void* elem, void* user)
{
  executor_t exec = (executor_t)elem;
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

  return exec;
}

// Joins the list of executors.
void
join_executors()
{
  list_foreach(executors, join_executor, NULL);
  list_free(executors);
}

void
create_executors(sync_data_t sync_data, int n)
{
  executors = list_make(n+1);
  for(int i = 0; i < n; i++)
    {
      executor_t exec = make_executor(sync_data);
      list_add(executors, exec);
      pthread_create(&exec->thread, NULL, executor_run, exec);
    }
}