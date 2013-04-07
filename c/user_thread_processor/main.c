#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <stdint.h>

#include "executor.h"
#include "notifier.h"
#include "processor.h"
#include "work_list.h"

processor_t proc1;
processor_t proc2;

int
fib(processor_t exec, int i)
{
  maybe_yield(exec);
  if (i < 2)
    return 1;
  else
    return fib(exec, i-1) + fib(exec, i-2);
}

void
task2(processor_t exec)
{
  int x;
  printf("Thread2 on the go\n");
  x = fib(exec, 38);
  printf("Result2: %d\n", x);
  printf("Thread2 finished\n");
}

void
task1(processor_t exec)
{
  int x;
  printf("Thread1 on the go\n");  
  x = fib(exec, 38);
  printf("Result1: %d\n", x);
  printf("Thread1 finished\n");
}

int
main(int argc, char **argv)
{
  /* printf("%d\n", fib(38));*/
  /* free_work_list(make_work_list()); */
  proc1 = make_processor();
  proc2 = make_processor();

  reset_stack_to (task1, proc1);
  reset_stack_to (task2, proc2);

  create_executors(1);

  add_work_item(work, proc1);
  add_work_item(work, proc2);

  {
    pthread_t notifier;
    notifier = create_notifier();
    setup_timer();

    pthread_join(notifier, NULL);
  }

  join_executors();

  free_work_list(work);
  free_processor(proc1);
  free_processor(proc2);

  printf("end of test\n");
  return 0;
}
