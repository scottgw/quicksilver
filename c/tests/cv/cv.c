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
#include "libqs/task_condition.h"
#include "libqs/sync_ops.h"

#define MAX_TASKS 20000
#define N 10000
#define NUM_WORKERS 256

task_mutex_t mutex;
task_condition_t cv;
uint32_t worker_count = 0;
int x = 0;

void
cv_task(processor_t proc, int64_t sign)
{

  /* printf("USER: %p worker started for %d \n", proc, sign); */
  for (int i = 0; i < N; i++)
    {
      /* printf("USER: %p %d -- locking\n", proc, sign); */
      task_mutex_lock(mutex, proc);
      /* printf("USER: %p %d -- locked\n", proc, sign); */
      while (x % 2 != sign)
        {
          /* printf("USER: %p %d -- waiting\n", proc, sign); */
          task_condition_wait(cv, mutex, proc);
        }
      x++;
      /* printf("USER: %p %d -- signaling\n", proc, sign); */
      task_condition_signal_all(cv, proc);
      /* printf("USER: %p %d -- unlocking\n", proc, sign); */
      task_mutex_unlock(mutex, proc);
    }

  printf("USER: %p worker finished for %d\n", proc, sign);

  if (__atomic_add_fetch(&worker_count, 1, __ATOMIC_SEQ_CST) == NUM_WORKERS)
    {
      printf("USER: %p %d final exit\n", proc, sign);
      exit(0);
    }
}

void
proc_main(processor_t proc)
{
  mutex = task_mutex_new();
  cv = task_condition_new();
  for (int64_t i = 0; i < NUM_WORKERS; i++)
    {
      processor_t worker_proc = proc_new(proc->task->sync_data);
      priv_queue_t q = proc_get_queue(proc, worker_proc);
      
      void ***args;
      clos_type_t *arg_types;
 
      closure_t clos =
        closure_new(cv_task,
                    closure_void_type(),
                    2,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      arg_types[1] = closure_pointer_type();
      
      *args[0] = worker_proc;
      *args[1] = (void*)(i % 2);

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
  proc_new_root(sync_data, proc_main);

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
