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

list_t executors;

static
void
switch_to_next_processor(executor_t exec)
{
  assert(list_size(exec->work) > 0);

  // take a new piece of work.
  exec->current_proc = list_take(exec->work);

  // switch to new processor
  exec->current_proc->state = TASK_RUNNING;

  yield_to_processor(exec, exec->current_proc);

  // If the came back finished, then remove it from the
  // work list.
  if(exec->current_proc->state != TASK_FINISHED)
    list_add(exec->work, exec->current_proc);
}


static
void
free_executor(executor_t exec)
{
  free (exec->ctx.uc_stack.ss_sp);
  free (exec);
}


static
void
executor_loop2(executor_t exec)
{
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
executor_loop(void* data)
{
  executor_t exec = data;
  ucontext_t loop_ctx;
  
  // volatile because we'll have to reload this after a
  // context switch by setcontext.
  int volatile flag = 0;
  getcontext(&loop_ctx);
  if (flag == 0)
    {
      void* stack = malloc(STACKSIZE);
      flag = 1;
      getcontext(&exec->ctx);

      exec->ctx.uc_stack.ss_sp   = stack + STACKSIZE;
      exec->ctx.uc_stack.ss_size = STACKSIZE;
      exec->ctx.uc_link = &loop_ctx;

      makecontext(&exec->ctx, (void (*)())executor_loop2, 1, exec);
      setcontext(&exec->ctx);
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
      pthread_create(&exec->thread, NULL, executor_loop, exec);
    }
}
