#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <pthread.h>
#include <glib.h>

#include <ucontext.h>

#include "executor.h"
#include "processor.h"
#include "work_list.h"
#include "notifier.h"

/* extern volatile int register_count; */
/* extern volatile int num_executors; */

static
void
get_next_processor(executor_t exec)
{
  /* printf("next proc: %d\n", work_list_size(exec->work)); */
  if (exec->current_proc != NULL)
    {
      add_work_item(exec->work, exec->current_proc);
    }
  assert(work_list_size(exec->work) > 0);
  exec->current_proc = take_work_item(exec->work);
  /* printf ("exec: %d in queue\n", work_list_size(work)); */
  assert (exec->current_proc != NULL);
}

static
void
switch_to_next_processor(executor_t exec)
{
  /* printf("switching\n"); */
  get_next_processor(exec);
  if (proc_running(exec->current_proc))
    {
      yield_to_processor(exec, exec->current_proc);
      if (exec->current_proc->is_done == 1)
        exec->current_proc = NULL;
    }
  else
    {
      proc_start(exec->current_proc, exec);
    }
}

static
void
executor_loop2(executor_t exec)
{
  
  printf("Executor running\n");
  while(1)
    {
      if (work_list_size(exec->work) > 0)
        switch_to_next_processor(exec);
      else
        {
          printf("no workers\n");
          /* free_work_list(work);*/
          pthread_exit(NULL);
        }
    }

}

static 
void*
executor_loop(void* data)
{
  executor_t exec = data;
  void* stack = malloc(STACKSIZE);

  getcontext(&exec->ctx);

  exec->ctx.uc_stack.ss_sp   = stack + STACKSIZE;
  exec->ctx.uc_stack.ss_size = STACKSIZE;

  makecontext(&exec->ctx, (void (*)())executor_loop2, 1, exec);
  setcontext(&exec->ctx);
  
  return NULL;
}


static
void
join_executor(gpointer elem, gpointer user)
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
  g_list_foreach(executors, join_executor, NULL);
}


void
create_executors(work_list_t work, int n)
{
  for(int i = 0; i < n; i++)
    {
      executor_t exec = make_executor();
      exec->work = work;
      executors = g_list_append(executors, exec);
      pthread_create(&exec->thread, NULL, executor_loop, exec);
    }
}
