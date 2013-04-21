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
#define N 40

task_mutex_t mutex;

void
task2(void* data)
{
  processor_t proc = (processor_t) data;
  yield_to_executor(proc);
  printf("Thread 2 on the go\n");
  task_mutex_lock(mutex, proc);
  printf("Thread 2 yielding for fun\n");
  yield_to_executor(proc);
  printf("Thread 2: back from yield\n");
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
  printf("Thread 1 yielding for fun\n");
  yield_to_executor(proc);
  printf("Thread 1: back from yield\n");
  task_mutex_unlock(mutex, proc);
  printf("Thread 1 finished\n");
}

int
main(int argc, char **argv)
{
  sync_data_t sync_data = sync_data_new(MAX_TASKS);
  processor_t proc1 = make_processor();
  processor_t proc2 = make_processor();
  mutex = task_mutex_new();

  reset_stack_to (task1, proc1);
  reset_stack_to (task2, proc2);

  sync_data_enqueue_runnable(sync_data, proc1);
  sync_data_enqueue_runnable(sync_data, proc2);

  create_executors(sync_data, 2);

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
