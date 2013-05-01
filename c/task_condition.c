#include "processor.h"

#include "queue_impl.h"
#include "task_mutex.h"
#include "task_condition.h"
#include "task.h"

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
task_condition_signal(task_condition_t cv)
{
  processor_t proc;

  if(queue_impl_dequeue(cv->wait_queue, (void**)&proc))
    {
      proc_wake(proc);
    }
}

void
task_condition_wait(task_condition_t cv, task_mutex_t mutex, processor_t proc)
{
  proc->task->state = TASK_WAITING;
  assert (queue_impl_enqueue(cv->wait_queue, proc));

  task_mutex_unlock(mutex, proc);
  yield_to_executor(proc);

  task_mutex_lock(mutex, proc);
}
