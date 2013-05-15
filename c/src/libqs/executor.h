#ifndef __EXECUTOR_H_
#define __EXECUTOR_H_

#include <pthread.h>

#include "processor.h"

#include "sync_ops.h"

struct executor
{
  task_t task;
  volatile bool done;
  processor_t current_proc;
  pthread_t thread;
  int id;
};

// Constructs the executor thread and adds the executor
// To the list of executors.
executor_t
make_executor(sync_data_t);

// Free the memory for the executor.
void
executor_free(executor_t);

// Join all the executors in 'executors'.
void
join_executors();

// Reschedule the currently executing processor.
void
executor_reschedule(executor_t);

// Creates 'n' executors and stores them in the 'executors' list.
void
create_executors(sync_data_t sync_data, int n);

#endif // __EXECUTOR_H_