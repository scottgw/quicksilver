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
#include "task_condition.h"

#include "sync_ops.h"

// FIXME: 16382 actually causes a segfault in makecontext for
// the executor, and it's not clear why lower values work OK.
//
// update, now it doesn't fail with large MAX_TASKS; that is disturbing.
#define MAX_TASKS 16
#define N 40

task_mutex_t mutex;
task_condition_t cv;

int global = 0;

void
task2(void* data)
{
  processor_t proc = (processor_t) data;
  yield_to_executor(proc);
  printf("Thread 2 on the go\n");
  task_mutex_lock(mutex, proc);

  while (global != 0)
    {
      printf("Thread 2 waiting for 0\n");
      task_condition_wait(cv, mutex, proc);
    }

  printf("Thread 2: found a 0\n");
  
  global++;
  task_condition_signal(cv);
  
  while (global != 2)
    {
      printf("Thread 2 waiting for a 2\n");
      task_condition_wait(cv, mutex, proc);
    }
  printf("Thread 2: found a 2\n");
  
  global++;
  task_condition_signal(cv);
  
  task_mutex_unlock(mutex, proc);
  printf("Thread 2 finished\n");
  sync_data_deregister_proc(proc->task->sync_data);
}

void
task1(void* data)
{
  processor_t proc = (processor_t) data;
  yield_to_executor(proc);
  printf("Thread 1 on the go\n");
  task_mutex_lock(mutex, proc);

  while (global != 1)
    {
      printf("Thread 1 waiting for 1\n");
      task_condition_wait(cv, mutex, proc);
    }
  printf("Thread 1: found a 1\n");

  global++;
  task_condition_signal(cv);

  while (global != 3)
    {
      printf("Thread 1 waiting for a 3\n");
      task_condition_wait(cv, mutex, proc);
    }
  printf("Thread 1: found a 3\n");

  global++;
  task_condition_signal(cv);

  task_mutex_unlock(mutex, proc);
  printf("Thread 1 finished\n");
  sync_data_deregister_proc(proc->task->sync_data);
}

int
main(int argc, char **argv)
{
  sync_data_t sync_data = sync_data_new(MAX_TASKS);
  processor_t proc1 = make_processor(sync_data);
  processor_t proc2 = make_processor(sync_data);
  mutex = task_mutex_new();
  cv = task_condition_new();

  reset_stack_to (task1, proc1);
  reset_stack_to (task2, proc2);

  sync_data_register_proc(sync_data);
  sync_data_register_proc(sync_data);

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
