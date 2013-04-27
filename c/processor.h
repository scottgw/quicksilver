#ifndef __PROCESSOR_H_
#define __PROCESSOR_H_

#include <stdio.h>
#include <stdbool.h>

#include "types.h"

#include "task_mutex.h"
#include "task_condition.h"

struct processor
{
  // Underlying task
  task_t task;

  // Current executor
  executor_t executor;

  // Queue of queues which the processor
  // will take requests from.
  bounded_queue_t qoq; 

  // Processor availability
  bool available;
  task_mutex_t mutex;
  task_condition_t cv;

  // Identifier
  int id;  
};

processor_t
make_processor();

void
reset_stack_to(void (*)(void*), processor_t);

void
proc_wake(processor_t proc);

bounded_queue_t
proc_make_private_queue(processor_t proc);

void
yield_to_processor(executor_t, processor_t);

void
yield_to_executor(processor_t);

void
free_processor(processor_t);

void
proc_sleep(processor_t, struct timespec);

int
proc_running(processor_t);

void
proc_start(processor_t, executor_t);

void
maybe_yield(processor_t, int);

#endif // __PROCESSOR_H_
