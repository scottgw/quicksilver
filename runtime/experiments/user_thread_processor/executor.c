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
  assert(list_size(exec->work) > 0);

  // take a new piece of work.
  processor_t proc = list_take(exec->work);
  proc->executor = exec;
  exec->current_proc = proc;

  // If this task is to finish, it should restore this executors context.
  proc->task->next = exec->task;

  yield_to(exec->task, proc->task);

  // If the came back finished, then remove it from the
  // work list.
  if(proc->task->state != TASK_FINISHED)
    list_add(exec->work, proc);
}

static
void
free_executor(executor_t exec)
{
  free (exec->task);
  free (exec);
}

static
void
executor_loop(void* data)
{
  executor_t exec = (executor_t) data;
  int volatile done = 0;
  printf("Executor running\n");
  while(done == 0)
    {
      if (list_size(exec->work) > 0)
        {
          switch_to_next_processor(exec);
        }
      else
        {
          printf("no workers\n");
          /* free_executor(exec); */
          done = 1;
        }
    }
}

static 
void*
executor_run(void* data)
{
  executor_t exec = data;
  ucontext_t loop_ctx;
  
  // volatile because we'll have to reload this after a
  // context switch by setcontext.
  int volatile flag = 0;
  getcontext(&loop_ctx);
  if (flag == 0)
    {
      flag = 1;
      exec->task->ctx.uc_link = &loop_ctx;
      task_set_func(exec->task, executor_loop, exec);
      task_run(exec->task);
    }
  printf("end exec loop\n");
  notifier_done = 1;
  return NULL;
}

static
void
join_executor(void* elem, void* user)
{
  executor_t exec = (executor_t)elem;
  pthread_join(exec->thread, NULL);
}


// Constructs the executor thread and adds the executor
// To the list of executors.
executor_t
make_executor()
{
  executor_t exec = malloc(sizeof(executor_t));
  exec->task = task_make();
  exec->current_proc = NULL;
  return exec;
}

// Joins the list of executors.
void
join_executors()
{
  list_foreach(executors, join_executor, NULL);
}

void
create_executors(list_t work, int n)
{
  executors = list_make();
  for(int i = 0; i < n; i++)
    {
      executor_t exec = make_executor();
      exec->work = work;
      list_add(executors, exec);
      pthread_create(&exec->thread, NULL, executor_run, exec);
    }
}
