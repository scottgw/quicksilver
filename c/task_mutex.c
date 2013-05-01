#include "task_mutex.h"

#include "sync_ops.h"
#include "processor.h"
#include "queue_impl.h"
#include "task.h"
#include "types.h"

#define INIT_WAIT_QUEUE_SIZE 16000

struct task_mutex 
{
  volatile uint32_t count;
  volatile processor_t owner;
  queue_impl_t wait_queue;
};

task_mutex_t
task_mutex_new()
{
  task_mutex_t mutex = (task_mutex_t)malloc(sizeof(struct task_mutex));
  mutex->count = 0;
  mutex->owner = NULL;
  mutex->wait_queue = queue_impl_new(INIT_WAIT_QUEUE_SIZE);

  return mutex;
}

void
task_mutex_free(task_mutex_t mutex)
{
  queue_impl_free(mutex->wait_queue);
  free (mutex);
}

void
task_mutex_lock(task_mutex_t mutex, processor_t proc)
{
 if (__sync_fetch_and_add(&mutex->count, 1) > 0)
    {
      // if the owner is already set then we add to the wait list 
      // and yield to the executor.
      proc->task->state = TASK_TRANSITION_TO_WAITING;
      assert(queue_impl_enqueue(mutex->wait_queue, proc));
      yield_to_executor(proc);
    }
 else
   {
     // if not, set the owner.
     __sync_synchronize();
     mutex->owner = proc;
   }
}

void
task_mutex_unlock(task_mutex_t mutex, processor_t proc)
{
  processor_t other_proc = NULL;
  assert(mutex->owner == proc);
  
  if (__sync_fetch_and_sub(&mutex->count, 1) > 1)
    {
      // if there's someone in the loop, spin to dequeue them from the wait-queue.
      // spinning is OK here because we only have to wait between when they
      // increased the counter and when the enqueue themselves which
      // is a finite amount of time.
      while(!queue_impl_dequeue(mutex->wait_queue, (void**)&other_proc));
      __sync_synchronize();
      while(other_proc->task->state != TASK_WAITING);
      mutex->owner = other_proc;
      other_proc->task->state = TASK_RUNNABLE;
      sync_data_enqueue_runnable(proc->task->sync_data, other_proc);
    }
  else
    {
      // If there were zero people here, then just set the owner to NULL
      __sync_synchronize();
      mutex->owner = NULL;
    }
}
