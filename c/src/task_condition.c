#include <assert.h>
#include <stdlib.h>

#include "internal/debug_log.h"
#include "libqs/sched_task.h"
#include "internal/queue_impl.h"
#include "internal/task_mutex.h"
#include "internal/task_condition.h"
#include "internal/task.h"

#define INIT_WAIT_QUEUE_SIZE 16000

struct task_condition
{
  // Invariant: all processors in the queue have their state as TASK_WAITING
  queue_impl_t wait_queue;
};

task_condition_t
task_condition_new()
{
  task_condition_t cv = (task_condition_t)malloc(sizeof(struct task_condition));

  cv->wait_queue = queue_impl_new(INIT_WAIT_QUEUE_SIZE);

  return cv;
}

void
task_condition_free(task_condition_t cv)
{
  queue_impl_free(cv->wait_queue);
  free(cv);
}

void
task_condition_signal(task_condition_t cv, sched_task_t curr_stask)
{
  sched_task_t stask;

  if(queue_impl_dequeue(cv->wait_queue, (void**)&stask))
    {
      stask_wake(stask, curr_stask->executor);
    }
}


void
task_condition_signal_all(task_condition_t cv, sched_task_t curr_stask)
{
  sched_task_t stask;

  while (queue_impl_dequeue(cv->wait_queue, (void**)&stask))
    {
      stask_wake(stask, curr_stask->executor);
    }
}


void
task_condition_wait(task_condition_t cv, task_mutex_t mutex, sched_task_t stask)
{
  volatile sched_task_t vstask = stask;
  assert(task_mutex_owner(mutex) == vstask);

  DEBUG_LOG(2, "%p waiting in cv %p\n", stask, cv);

  task_set_state(vstask->task, TASK_TRANSITION_TO_WAITING);
  bool success = queue_impl_enqueue(cv->wait_queue, vstask);
  assert(success);

  task_mutex_unlock(mutex, vstask);
  DEBUG_LOG(2, "%p moving to executor in cv %p\n", stask, cv);
  stask_yield_to_executor(vstask);
  DEBUG_LOG(2, "%p resumed in cv %p\n", stask, cv);
  task_mutex_lock(mutex, vstask);
}
