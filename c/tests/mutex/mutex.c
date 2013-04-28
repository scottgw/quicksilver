#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <stdint.h>

#include "executor.h"
#include "notifier.h"
#include "processor.h"
#include "list.h"
#include "task.h"
#include "task_mutex.h"

#include "sync_ops.h"

// FIXME: 16382 actually causes a segfault in makecontext for
// the executor, and it's not clear why lower values work OK.
//
// update, now it doesn't fail with large MAX_TASKS; that is disturbing.
#define MAX_TASKS 16
#define N 40

task_mutex_t mutex;

void
task2(void* data)
{
  processor_t proc = (processor_t) data;
  yield_to_executor(proc);
  printf("Thread 2 on the go\n");
  task_mutex_lock(mutex, proc);
  printf("Thread 2 critical section start\n");
  yield_to_executor(proc);
  printf("Thread 2 critical section end\n");
  task_mutex_unlock(mutex, proc);
  printf("Thread 2 finished\n");
}

void
task1(void* data)
{
  processor_t proc = (processor_t) data;
  yield_to_executor(proc);
  printf("Thread 1 on the go\n");
  task_mutex_lock(mutex, proc);
  printf("Thread 1 critical section start\n");
  yield_to_executor(proc);
  printf("Thread 1 critical section end\n");
  task_mutex_unlock(mutex, proc);
  printf("Thread 1 finished\n");
}

void
proc_main(processor_t proc)
{
  mutex = task_mutex_new();

  processor_t *proc1_ptr = malloc(sizeof(processor_t));
  processor_t *proc2_ptr = malloc(sizeof(processor_t));
  processor_t proc1 = make_processor(proc->task->sync_data);
  processor_t proc2 = make_processor(proc->task->sync_data);

  *proc1_ptr = proc1;
  *proc2_ptr = proc2;

  bounded_queue_t q1 = proc_make_private_queue(proc1);
  bounded_queue_t q2 = proc_make_private_queue(proc2);

  ffi_cif *cif1 = (ffi_cif*)malloc(sizeof(ffi_cif));
  ffi_type **arg_types1 = (ffi_type**)malloc(sizeof(ffi_type*));
  arg_types1[0] = &ffi_type_pointer;
  assert
    (ffi_prep_cif
     (cif1, FFI_DEFAULT_ABI, 1, &ffi_type_void, arg_types1) ==
     FFI_OK);

  ffi_cif *cif2 = (ffi_cif*)malloc(sizeof(ffi_cif));
  ffi_type **arg_types2 = (ffi_type**)malloc(sizeof(ffi_type*));
  arg_types2[0] = &ffi_type_pointer;
  assert
    (ffi_prep_cif
     (cif2, FFI_DEFAULT_ABI, 1, &ffi_type_void, arg_types2) ==
     FFI_OK);

  void **args1 = (void**)malloc(sizeof(processor_t*));
  args1[0] = proc1_ptr;
  void **args2 = (void**)malloc(sizeof(processor_t*));
  args2[0] = proc2_ptr;

  closure_t clos1 = closure_new(cif1, task1, 1, args1);
  closure_t clos2 = closure_new(cif2, task2, 1, args2);

  enqueue_closure(q1, clos1);
  enqueue_closure(q1, NULL);

  enqueue_closure(q2, clos2);
  enqueue_closure(q2, NULL);

  proc_shutdown(proc1);
  proc_shutdown(proc2);
}

int
main(int argc, char **argv)
{
  sync_data_t sync_data = sync_data_new(MAX_TASKS);
  processor_t *proc_ptr = malloc(sizeof(processor_t));
  processor_t proc = make_processor(sync_data);
  *proc_ptr = proc;

  create_executors(sync_data, 2);

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
