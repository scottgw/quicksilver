#ifndef __EXECUTOR_H_
#define __EXECUTOR_H_

#include <pthread.h>

#include "../libqs/types.h"

struct executor
{
  sched_task_t stask;
  volatile bool done;
  sched_task_t current_stask;
  pthread_t thread;
  int id;

  uint32_t backoff_us;

  ws_deque_t local_deque;
};

// Constructs the executor thread and adds the executor
// To the list of executors.
executor_t
make_executor(sync_data_t);

bool
exec_pop (executor_t exec, sched_task_t *proc);

void
exec_push (executor_t exec, sched_task_t proc);

bool
exec_steal (executor_t victim_exec, sched_task_t *proc);

sched_task_t
exec_get_work(executor_t exec, uint32_t attempts);

void
exec_step_previous(executor_t exec, sched_task_t ignore_proc);

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
