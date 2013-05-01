#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <stdint.h>
#include <ffi.h>

#include "bounded_queue.h"
#include "executor.h"
#include "notifier.h"
#include "processor.h"
#include "task.h"
#include "list.h"
#include "task_mutex.h"
#include "private_queue.h"

#include "sync_ops.h"

#define MAX_TASKS 20000
#define N 32
#define ITERS 20000

int x = 0;
volatile int num_finished = 0;

void
action(processor_t proc)
{
  /* printf("aciton: %d\n", x); */
  x++;
}

void
worker(processor_t proc, processor_t shared)
{
  void ***args;
  clos_type_t *arg_types;
  priv_queue_t q = priv_queue_new(shared);

  for (int i = 0; i < ITERS; i++)
    { 
      closure_t clos =
        closure_new(action,
                    closure_void_type(),
                    1,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      
      *args[0] = shared;

      priv_queue_lock(q, shared, proc);
      priv_queue_routine(q, clos, proc);
      priv_queue_unlock(q, proc);
    }

  priv_queue_shutdown(q, shared, proc);
  proc_shutdown(proc, proc);

  printf("worker shutdown\n");
  if( __sync_add_and_fetch(&num_finished, 1) == N)
    {
      printf("shared shutdown %p\n", shared);
      proc_shutdown(shared, proc);
    }
}

void
proc_main(processor_t proc)
{
  processor_t shared = make_processor(proc->task->sync_data);
  
  for (int i = 0; i < N; i++)
    {
      processor_t worker_proc = make_processor(proc->task->sync_data);
      priv_queue_t q = priv_queue_new(worker_proc);
      
      void ***args;
      clos_type_t *arg_types;
 
      closure_t clos =
        closure_new(worker,
                    closure_void_type(),
                    2,
                    &args,
                    &arg_types);
      
      arg_types[0] = closure_pointer_type();
      arg_types[1] = closure_pointer_type();
      
      *args[0] = worker_proc;
      *args[1] = shared;

      priv_queue_lock(q, worker_proc, proc);
      priv_queue_routine(q, clos, proc);
      priv_queue_unlock(q, proc);

      priv_queue_shutdown(q, worker_proc, proc);
    }
}

int
main(int argc, char **argv)
{
  sync_data_t sync_data = sync_data_new(MAX_TASKS);
  processor_t proc = make_processor(sync_data);

  create_executors(sync_data, 4);

  priv_queue_t q = priv_queue_new(proc);

  void ***args;
  clos_type_t *arg_types;

  closure_t clos =
    closure_new(proc_main,
                closure_void_type(),
                1,
                &args,
                &arg_types);

  arg_types[0] = closure_pointer_type();
  *args[0] = proc;

  priv_queue_lock(q, proc, proc);
  priv_queue_routine(q, clos, proc);
  priv_queue_unlock(q, proc);

  printf("main shutdown\n");
  proc_shutdown(proc, proc);

  {
    notifier_t notifier = notifier_spawn(sync_data);
    notifier_join(notifier);
  }

  join_executors();
  priv_queue_free(q);


  printf ("x is: %d\n", x);
  sync_data_free(sync_data);
  return 0;
}
