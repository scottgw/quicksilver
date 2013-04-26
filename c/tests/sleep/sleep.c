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


void
task1(void* data)
{
  processor_t proc = (processor_t)data;
  struct timespec ts;
  ts.tv_sec = 1;
  ts.tv_nsec = 0;
  printf("Pre sleep 1\n");
  proc_sleep(proc, ts);
  printf("post sleep 1\n");
  proc_sleep(proc, ts);
  printf("post sleep 1\n");
  sync_data_deregister_proc(proc->task->sync_data);
}

void
task2(void* data)
{
  processor_t proc = (processor_t)data;
  struct timespec ts;
  ts.tv_sec = 2;
  ts.tv_nsec = 0;
  printf("Pre sleep 2\n");
  proc_sleep(proc, ts);
  printf("post sleep 2\n");
  proc_sleep(proc, ts);
  printf("post sleep 2\n");
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
