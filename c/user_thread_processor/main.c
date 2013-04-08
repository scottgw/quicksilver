#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <stdint.h>

#include "executor.h"
#include "notifier.h"
#include "processor.h"
#include "list.h"

#define N 30

processor_t proc1;
processor_t proc2;

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
  list_t work = list_make();

  /* printf("%d\n", fib(NULL, N)); */
  /* printf("%d\n", fib(NULL, N)); */

  proc1 = make_processor();
  proc2 = make_processor();

  reset_stack_to (task1, proc1);
  reset_stack_to (task2, proc2);

  list_add(work, proc1);
  list_add(work, proc2);

  create_executors(work, 1);

  {
    pthread_t notifier = create_notifier();
    pthread_join(notifier, NULL);
  }

  join_executors();

  printf("post join\n");

  list_free(work);
  free_processor(proc1);
  free_processor(proc2);

  printf("end of test\n");

  return 0;
}
