#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <stdint.h>

#include "bounded_queue.h"
#include "executor.h"
#include "maybe.h"
#include "notifier.h"
#include "processor.h"
#include "task.h"
#include "list.h"
#include "task_mutex.h"
#include "closure.h"

#include "sync_ops.h"

// FIXME: 16382 actually causes a segfault in makecontext for
// the executor, and it's not clear why lower values work OK.
//
// update, now it doesn't fail with large MAX_TASKS; that is disturbing.
#define MAX_TASKS 16
#define N 40

int
fib(processor_t proc, int i)
{
  maybe_yield(proc, i);
  if (i < 2)
    return 1;
  else
    return fib(proc, i-1) + fib(proc, i-2);
}


void
task2(void* data)
{
  int x;
  processor_t proc = (processor_t)data;
  printf("Thread2 on the go\n");
  x = fib(proc, N);
  printf("Result2: %d\n", x);
  printf("Thread2 finished\n");
  sync_data_deregister_proc(proc->task->sync_data);
}

void
task1(void* data)
{
  int x;
  processor_t proc = (processor_t)data;
  printf("Thread1 on the go\n");
  x = fib(proc, N);
  printf("Result1: %d\n", x);
  printf("Thread1 finished\n");
  sync_data_deregister_proc(proc->task->sync_data);
}


void
proc_main(void* data)
{
  processor_t proc = (processor_t) data;

  processor_t proc1 = make_processor(proc->task->sync_data);
  processor_t proc2 = make_processor(proc->task->sync_data);

  bounded_queue_t q1 = proc_make_private_queue(proc1);
  bounded_queue_t q2 = proc_make_private_queue(proc2);

  closure_t clos1 = closure_new(task1, proc1);
  closure_t clos2 = closure_new(task2, proc2);

  maybe_t m1 = (maybe_t) malloc(sizeof(struct maybe));
  maybe_t m2 = (maybe_t) malloc(sizeof(struct maybe));

  m1->load = clos1;
  m2->load = clos2;

  bqueue_enqueue(q1, m1);
  bqueue_enqueue(q2, m2);

}

int
main(int argc, char **argv)
{
  sync_data_t sync_data = sync_data_new(MAX_TASKS);
  processor_t proc = make_processor(sync_data);

  create_executors(sync_data, 2);

  bounded_queue_t q = proc_make_private_queue(proc);
  closure_t clos = closure_new(proc_main, proc);
  maybe_t m = (maybe_t) malloc(sizeof(struct maybe));
  m->load = clos;

  bqueue_enqueue(q, m);

  {
    notifier_t notifier = notifier_spawn(sync_data);
    notifier_join(notifier);
  }

  join_executors();

  return 0;
}
