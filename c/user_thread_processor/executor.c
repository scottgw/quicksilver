#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/time.h>
#include <pthread.h>
#include <glib.h>

#include "executor.h"
#include "processor.h"
#include "work_list.h"
#include "notifier.h"

struct executor
{
  pthread_t thread;
};

/* extern volatile int register_count; */
/* extern volatile int num_executors; */

static
void
get_next_processor(executor_t exec)
{
  if (current_proc != NULL)
    {
      add_work_item(work, current_proc);
    }
  current_proc = take_work_item(work);
  printf ("exec: %d in queue\n", work_list_size(work));
  assert (current_proc != NULL);
}

static
void
switch_to_next_processor(executor_t exec)
{
  get_next_processor(exec);
  yield_to_processor(current_proc);
  assert (current_proc != NULL);
}

static 
void*
executor_loop(void* data)
{
  executor_t exec = data;
  printf("Executor running\n");
  while(1)
    {
      if (work_list_size(work) > 0)
        switch_to_next_processor(exec);
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
  executor_t exec = (executor_t)elem;
  pthread_join(exec->thread, NULL);
}


// Constructs the executor thread and adds the executor
// To the list of executors.
executor_t
make_executor()
{
  executor_t exec = malloc(sizeof(executor_t));
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

  for(int i = 0; i < n; i++)
    {
      executor_t exec = make_executor();
      executors = g_list_append(executors, exec);
      pthread_create(&exec->thread, NULL, executor_loop, exec);
    }
}
