#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <stdint.h>

#include "executor.h"
#include "notifier.h"
#include "processor.h"
#include "list.h"
#include "task_mutex.h"

#include "sync_ops.h"

// FIXME: 16382 actually causes a segfault in makecontext for
// the executor, and it's not clear why lower values work OK.
//
// update, now it doesn't fail with large MAX_TASKS; that is disturbing.
#define MAX_TASKS 16
#define N 1000000

void
task2(void* data)
{
  processor_t proc = (processor_t)data;
  printf("Thread 2 on the go\n");
  for (int i = 0; i < N; i++)
    { 
      yield_to_executor(proc);
    }
  printf("Thread 2 finished\n");
  sync_data_deregister_proc(proc->task->sync_data);
}

void
task1(void* data)
{
  processor_t proc = (processor_t)data;
  printf("Thread 1 on the go\n");  
  for (int i = 0; i < N; i++)
    { 
      yield_to_executor(proc);
    }
  printf("Thread 1 finished\n");
  sync_data_deregister_proc(proc->task->sync_data);
}

int
main(int argc, char **argv)
{
  sync_data_t sync_data = sync_data_new(MAX_TASKS);
  processor_t proc1 = make_processor(sync_data);
  processor_t proc2 = make_processor(sync_data);

  reset_stack_to (task1, proc1);
  reset_stack_to (task2, proc2);

  sync_data_register_proc(sync_data);
  sync_data_register_proc(sync_data);

  sync_data_enqueue_runnable(sync_data, proc1);
  sync_data_enqueue_runnable(sync_data, proc2);

  create_executors(sync_data, 1);

  {
    notifier_t notifier = notifier_spawn(sync_data);
    pthread_join(notifier->thread, NULL);
  }

  join_executors();

  printf("post join\n");

  /* list_free(work); */
  free_processor(proc1);
  free_processor(proc2);

  printf("end of test\n");

  return 0;
}
