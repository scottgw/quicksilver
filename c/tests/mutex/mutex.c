#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <stdint.h>

#include "libqs/executor.h"
#include "libqs/notifier.h"
#include "libqs/processor.h"
#include "libqs/private_queue.h"
#include "libqs/task.h"
#include "libqs/task_mutex.h"
#include "libqs/sync_ops.h"

#define MAX_TASKS 20000
#define N 500000
#define NUM_WORKERS 64

task_mutex_t mutex;
uint32_t worker_count = 0;
int x = 0;

void
locking_task(processor_t proc)
{
  for (int i = 0; i < N; i++)
    {
      task_mutex_lock(mutex, proc);
      x++;
      task_mutex_unlock(mutex, proc);
    }
  printf("USER: %p worker  finished\n", proc);
  if (__atomic_add_fetch(&worker_count, 1, __ATOMIC_SEQ_CST) == NUM_WORKERS)
    {
      exit(0);
    }
}

void
proc_main(processor_t proc)
{
  mutex = task_mutex_new();
  for (int i = 0; i < NUM_WORKERS; i++)
    {
      processor_t worker_proc = proc_new(proc->task->sync_data);
      priv_queue_t q = proc_get_queue(proc, worker_proc);
      
      void ***args;
      clos_type_t *arg_types;
 
      closure_t clos =
        closure_new(locking_task,
                    closure_void_type(),
                    1,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      
      *args[0] = worker_proc;

      priv_queue_lock(q, proc);
      priv_queue_routine(q, clos, proc);
      priv_queue_unlock(q, proc);
    }

  printf("USER: %p root finished\n", proc);
}

int
main(int argc, char **argv)
{
  
  sync_data_t sync_data = sync_data_new(MAX_TASKS);
  processor_t proc = proc_new_root(sync_data, proc_main);

  create_executors(sync_data, 4);

  {
    notifier_t notifier = notifier_spawn(sync_data);
    notifier_join(notifier);
  }

  join_executors();

  printf ("x is: %d\n", x);
  sync_data_free(sync_data);
  return 0;
}
