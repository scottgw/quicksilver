#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <pthread.h>

#include <ucontext.h>

#include "executor.h"
#include "processor.h"
#include "list.h"
#include "notifier.h"
#include "task.h"

list_t executors;

static
void
switch_to_next_processor(executor_t exec)
{
  // take a new piece of work.
  processor_t proc = sync_data_dequeue_runnable(exec->task->sync_data);
  if (proc != NULL)
    {
      proc->executor = exec;
      exec->current_proc = proc;

      // If this task is to finish, it should restore this executors context.
      proc->task->next = exec->task;

      yield_to(exec->task, proc->task);

      // If the came back finished, then remove it from the
      // work list.
      if(proc->task->state == TASK_RUNNABLE)
        sync_data_enqueue_runnable(exec->task->sync_data, proc);
    }
  else
    {
      exec->done = true;
    }
}

void
executor_free(executor_t exec)
{
  /* FIXME: this leads to a double-free, it's totally unclear why */
  /* task_free(exec->task); */
  /* free(exec); */
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
  /* executor_free(exec); */
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
  printf("calling join_executor\n");
  executor_t exec = (executor_t)elem;
  pthread_join(exec->thread, NULL);
  executor_free(exec);
}

// Constructs the executor thread and adds the executor
// To the list of executors.
executor_t
make_executor(sync_data_t sync_data)
{
  executor_t exec = malloc(sizeof(executor_t));
  exec->task = task_make(sync_data);
  exec->current_proc = NULL;
  return exec;
}

// Joins the list of executors.
void
join_executors()
{
  list_foreach(executors, join_executor, NULL);
  /* FIXME: this leads to a double-free, no idea why */
  /* list_free(executors); */
}

void
create_executors(sync_data_t sync_data, int n)
{
  executors = list_make(n);
  for(int i = 0; i < n; i++)
    {
      executor_t exec = make_executor(sync_data);
      // exec->work = work;
      list_add(executors, exec);
      pthread_create(&exec->thread, NULL, executor_run, exec);
    }
}
