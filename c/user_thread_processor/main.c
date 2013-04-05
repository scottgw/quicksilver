#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <stdint.h>

#include "executor.h"
#include "notifier.h"
#include "processor.h"
#include "work_list.h"

processor_t *proc1;
processor_t *proc2;

int
fib(int i)
{
  if (i < 2)
    return 1;
  else
    return fib(i-1) + fib(i-2);
}

void
task2(user_stack_t starter)
{
  STARTSTACK(starter);
  printf("Thread2 on the go\n");
  printf("Result2: %d\n", fib(40));
  printf("Thread2 finished\n");
  ENDSTACK(starter);
}


void
task1(user_stack_t starter)
{
  SWAPSTACK(starter);
  printf("Thread1 on the go\n");  
  printf("Result1: %d\n", fib(40));  
  printf("Thread1 finished\n");
  ENDSTACK(starter);
}

int
main(int argc, char **argv)
{
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
