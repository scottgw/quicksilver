#include "processor.h"

#include "task_mutex.h"
#include "task_condition.h"
#include "task.h"

#define INIT_WAIT_QUEUE_SIZE 16

struct task_condition
{
  // Invariant: all processors in the queue have their state as TASK_WAITING
  conc_queue_t wait_queue;
};

task_condition_t
task_condition_new()
{
  task_condition_t cv = (task_condition_t)malloc(sizeof(struct task_condition));

  assert(lfds611_queue_new(&cv->wait_queue, INIT_WAIT_QUEUE_SIZE) == 1);

  return cv;
}

void
task_condition_free(task_condition_t cv)
{
  lfds611_queue_delete(cv->wait_queue, NULL, NULL);
  free(cv);
}

void
task_condition_signal(task_condition_t cv)
{
  processor_t proc;

  if(lfds611_queue_dequeue(cv->wait_queue, (void**)&proc) == 1)
    {
      proc_wake(proc);
    }
}

void
task_condition_wait(task_condition_t cv, task_mutex_t mutex, processor_t proc)
{
  proc->task->state = TASK_WAITING;
  assert (lfds611_queue_guaranteed_enqueue(cv->wait_queue, proc) == 1);

  task_mutex_unlock(mutex, proc);
  yield_to_executor(proc);

  task_mutex_lock(mutex, proc);
}
