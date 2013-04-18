#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <stdint.h>

#include "executor.h"
#include "notifier.h"
#include "processor.h"
#include "list.h"

#include "sync_ops.h"

// FIXME: 16382 actually causes a segfault in makecontext for
// the executor, and it's not clear why lower values work OK.
//
// update, now it doesn't fail with large MAX_TASKS; that is disturbing.
#define MAX_TASKS 16
#define N 40

__attribute__ ((noinline))
static
void
false_yield(processor_t proc, int i)
{
  return;
}

int
fibY(processor_t proc, int i)
{
  maybe_yield(proc, i);
  if (i < 2)
    return 1;
  else
    return fibY(proc, i-1) + fibY(proc, i-2);
}


int
fib(processor_t proc, int i)
{
  false_yield(proc, i);
  if (i < 2)
    return 1;
  else
    return fib(proc, i-1) + fib(proc, i-2);
}

void
task3(void* data)
{
  processor_t proc = (processor_t)data;
  struct timespec ts;
  ts.tv_sec = 1;
  ts.tv_nsec = 0;
  printf("Pre sleep\n");
  proc_sleep(proc, ts);
  printf("post sleep 1\n");
  proc_sleep(proc, ts);
  printf("post sleep 2\n");
}

void
task2(void* data)
{
  int x;
  printf("Thread2 on the go\n");
  x = fibY((processor_t) data, N);
  printf("Result2: %d\n", x);
  printf("Thread2 finished\n");
}

void
task1(void* data)
{
  int x;
  printf("Thread1 on the go\n");  
  x = fibY((processor_t) data, N);
  printf("Result1: %d\n", x);
  printf("Thread1 finished\n");
}

int
main(int argc, char **argv)
{
  sync_data_t sync_data = sync_data_new(MAX_TASKS);
  processor_t proc1;
  processor_t proc2;
  processor_t proc3;


  /* printf("%d\n", fib(NULL, N)); */
  /* printf("%d\n", fib(NULL, N)); */

  proc1 = make_processor();
  proc2 = make_processor();
  proc3 = make_processor();

  reset_stack_to (task1, proc1);
  reset_stack_to (task2, proc2);
  reset_stack_to (task3, proc3);

  sync_data_enqueue_runnable(sync_data, proc1);
  sync_data_enqueue_runnable(sync_data, proc2);
  sync_data_enqueue_runnable(sync_data, proc3);

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
