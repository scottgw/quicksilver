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

// FIXME: 16382 actually causes a segfault in makecontext for
// the executor, and it's not clear why lower values work OK.
//
// update, now it doesn't fail with large MAX_TASKS; that is disturbing.
#define MAX_TASKS 16
#define N 100

int
query(processor_t proc, int x)
{
  return 42 + x;
}

void
proc_main(processor_t proc)
{
  processor_t proc1 = make_processor(proc->task->sync_data);
  priv_queue_t q1 = priv_queue_new(proc1);

  void ***args;
  clos_type_t *arg_types;
  
  int x = 0;
  for (int i = 0; i < 1000000; i++)
    {
      closure_t clos1 =
        closure_new(query,
                    closure_sint_type(),
                    2,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      arg_types[1] = closure_sint_type();

      *args[0] = proc1;
      *args[1] = 10;

      int old_x = x;
      priv_queue_function(q1, clos1, &x, proc);
      x += old_x;
    }
  printf("wait value: %d\n", x);
  priv_queue_unlock(q1);
  proc_shutdown(proc1);
  priv_queue_free(q1);
}

int
main(int argc, char **argv)
{
  sync_data_t sync_data = sync_data_new(MAX_TASKS);
  processor_t proc = make_processor(sync_data);

  create_executors(sync_data, 1);

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

  priv_queue_routine(q, clos);
  priv_queue_unlock(q);

  proc_shutdown(proc);
  priv_queue_free(q);

  {
    notifier_t notifier = notifier_spawn(sync_data);
    notifier_join(notifier);
  }

  join_executors();

  sync_data_free(sync_data);
  return 0;
}
