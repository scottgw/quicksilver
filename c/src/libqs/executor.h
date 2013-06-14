#ifndef __EXECUTOR_H_
#define __EXECUTOR_H_

#include <pthread.h>

#include "processor.h"

#include "sync_ops.h"
#include "types.h"

struct executor
{
  task_t task;
  volatile bool done;
  processor_t current_proc;
  pthread_t thread;
  int id;

  uint32_t backoff_us;

  ws_deque_t local_deque;
};

// Constructs the executor thread and adds the executor
// To the list of executors.
executor_t
make_executor(sync_data_t);

void
exec_push (executor_t exec, processor_t proc);

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
