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
query(processor_t proc)
{
  return 42;
}

void
proc_main(processor_t proc)
{
  int x;
  
  processor_t *proc1_ptr = malloc(sizeof(processor_t));
  processor_t proc1 = make_processor(proc->task->sync_data);

  *proc1_ptr = proc1;

  priv_queue_t q1 = priv_queue_new(proc1);

  ffi_cif *cif1 = (ffi_cif*)malloc(sizeof(ffi_cif));
  ffi_type **arg_types1 = (ffi_type**)malloc(sizeof(ffi_type*));
  arg_types1[0] = &ffi_type_pointer;
  assert
    (ffi_prep_cif
     (cif1, FFI_DEFAULT_ABI, 1, &ffi_type_sint, arg_types1) ==
     FFI_OK);

  void **args1 = (void**)malloc(sizeof(processor_t*));
  args1[0] = proc1_ptr;

  closure_t clos1 = closure_new(cif1, query, 1, args1);

  priv_queue_function(q1, clos1, &x, proc);
  
  printf("wait value: %d\n", x);

  priv_queue_unlock(q1);
  proc_shutdown(proc1);
  priv_queue_free(q1);
}

int
main(int argc, char **argv)
{
  sync_data_t sync_data = sync_data_new(MAX_TASKS);
  processor_t *proc_ptr = malloc(sizeof(processor_t));
  processor_t proc = make_processor(sync_data);
  *proc_ptr = proc;

  create_executors(sync_data, 1);

  bounded_queue_t q = proc_make_private_queue(proc);

  ffi_cif *cif = (ffi_cif*)malloc(sizeof(ffi_cif));
  ffi_type **arg_types = (ffi_type**)malloc(sizeof(ffi_type*));
  arg_types[0] = &ffi_type_pointer;
  ffi_prep_cif(cif, FFI_DEFAULT_ABI, 1, &ffi_type_void, arg_types);

  void **args = (void**)malloc(sizeof(processor_t*));
  args[0] = proc_ptr;
  closure_t clos = closure_new(cif, proc_main, 1, args);

  enqueue_closure(q, clos);
  enqueue_closure(q, NULL);
  proc_shutdown(proc);

  {
    notifier_t notifier = notifier_spawn(sync_data);
    notifier_join(notifier);
  }

  join_executors();

  sync_data_free(sync_data);
  return 0;
}
