#ifndef __PROCESSOR_H_
#define __PROCESSOR_H_

#include <stdio.h>
#include <stdbool.h>

#include "types.h"

#include "closure.h"
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
  volatile processor_t last_waiter;
  task_mutex_t mutex;
  task_condition_t cv;

  // Identifier
  int id;  
};

processor_t
proc_new(sync_data_t sync_data);

processor_t
proc_new_from_other(processor_t other_proc);

processor_t
proc_new_root(sync_data_t sync_data, void (*root)(processor_t));

/* Scheduled for deletion */
/* void */
/* reset_stack_to(void (*)(void*), processor_t); */

void
proc_wake(processor_t proc);

void
proc_yield_to_other(executor_t, processor_t);

void
proc_yield_to_executor(processor_t);

void
proc_wait_for_available(processor_t waitee, processor_t waiter);

void
proc_free(processor_t);

void
proc_sleep(processor_t, struct timespec);

int
proc_running(processor_t);

void
proc_start(processor_t, executor_t);

void
proc_shutdown(processor_t, processor_t);

void
proc_maybe_yield(processor_t);

#endif // __PROCESSOR_H_
