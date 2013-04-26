#include "task_mutex.h"

#include "sync_ops.h"
#include "processor.h"
#include "types.h"

#define INIT_WAIT_QUEUE_SIZE 16

struct task_mutex 
{
  volatile uint32_t count;
  volatile processor_t owner;
  conc_queue_t wait_queue;
};

task_mutex_t
task_mutex_new()
{
  task_mutex_t mutex = (task_mutex_t)malloc(sizeof(struct task_mutex));
  mutex->count = 0;
  mutex->owner = NULL;
  assert(lfds611_queue_new(&mutex->wait_queue, INIT_WAIT_QUEUE_SIZE) == 1);

  return mutex;
}

void
task_mutex_free(task_mutex_t mutex)
{
  lfds611_queue_delete(mutex->wait_queue, NULL, NULL);
  free (mutex);
}

void
task_mutex_lock(task_mutex_t mutex, processor_t proc)
{
 if (__sync_fetch_and_add(&mutex->count, 1) > 0)
    {
      // if the owner is already set then we add to the wait list 
      // and yield to the executor.
      proc->task->state = TASK_WAITING;
      lfds611_queue_enqueue(mutex->wait_queue, proc);
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
      while(lfds611_queue_dequeue(mutex->wait_queue, (void**)&other_proc) != 1);
      __sync_synchronize();
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
