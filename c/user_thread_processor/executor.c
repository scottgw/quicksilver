#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <pthread.h>
#include <glib.h>

#include "executor.h"
#include "processor.h"
#include "work_list.h"

static
processor_t*
get_next_processor()
{
  processor_t* proc;
  if (current_proc != NULL)
    {
      add_work_item(work, current_proc);
    }

  proc = (processor_t*) take_work_item(work);
  current_proc = proc;
  assert (proc != NULL);
  return proc;
}

static
void
execute_proc(processor_t *proc)
{
  assert (proc != NULL);

  SWAPPROC(proc);

  /* if (x == 0) // subtask finished naturally, so take it out of the work list */
  /*   { */
  /*     current_proc = NULL; */
  /*   } */
}

static
void
switch_to_next_processor()
{
  processor_t *proc = get_next_processor();
  execute_proc(proc);
}

static
void
executor_usr1_handler(int x)
{
  printf("USR1 Handled: switching processors\n");
  switch_to_next_processor();
  // assert(0 && "executor: switch to another processor");
  // FIXME: actually switch to another processor.
  // Not sure how to actually do this, as we're under
  // another control stack right now; sigsetjmp siglongjmp?
}

// The USR1 signal is used to tell this executor
// to switch to another processor.
static
void
executor_setup_usr1_handler()
{
  struct sigaction usr_action;
  sigset_t mask;
  sigemptyset(&mask);

  usr_action.sa_handler = executor_usr1_handler;
  usr_action.sa_mask    = mask;
  usr_action.sa_flags   = 0;

  sigaction (SIGUSR1, &usr_action, NULL);
}

// Block the alarm signal, there will be another
// thread that will forward this signal to us when
// it arises for rescheduling.
static
void
block_sigalrm()
{
  sigset_t mask;
  sigemptyset(&mask);
  sigaddset(&mask, SIGALRM);
  sigaddset(&mask, SIGINT);

  pthread_sigmask(SIG_BLOCK, &mask, NULL);
}

static 
void*
executor_loop(void* data)
{
  block_sigalrm();
  executor_setup_usr1_handler();
  printf("Executor running\n");
  while(1)
    {
      if (work_list_size(work) > 0)
        switch_to_next_processor();
      else
        {
          /* free_work_list(work);*/
          pthread_exit(NULL);
        }
    }
  return NULL;
}


static
void
join_executor(gpointer elem, gpointer user)
{
  executor_t *exec = (executor_t*)elem;
  pthread_join(exec->thread, NULL);
}


// Constructs the executor thread and adds the executor
// To the list of executors.
executor_t*
make_executor()
{
  executor_t *exec = malloc(sizeof(executor_t));
  return exec;

}

// Joins the list of executors.
void
join_executors()
{
  g_list_foreach(executors, join_executor, NULL);
}

void
create_executors(int n)
{
  work = make_work_list();
  current_proc = NULL;

  // This is conceivably being run from the
  // main thread, so we should block the alarm
  // signal we expect the notifier thread to pick up.
  block_sigalrm();

  for(int i = 0; i < n; i++)
    {
      executor_t *exec = make_executor();
      executors = g_list_append(executors, exec);
      pthread_create(&exec->thread, NULL, executor_loop, NULL);
    }
}
