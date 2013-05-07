#include <assert.h>
#include <stdlib.h>

#include "libqs/task_mutex.h"

#include "libqs/sync_ops.h"
#include "libqs/processor.h"
#include "libqs/queue_impl.h"
#include "libqs/task.h"
#include "libqs/types.h"

#define INIT_WAIT_QUEUE_SIZE 16000

struct task_mutex 
{
  volatile uint32_t count;
  volatile processor_t owner;
  volatile queue_impl_t wait_queue;
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

processor_t
task_mutex_owner(task_mutex_t mutex)
{
  return mutex->owner;
}


/* void */
/* task_mutex_lock(task_mutex_t mutex, processor_t proc) */
/* { */
/*   if (__sync_fetch_and_add(&mutex->count, 1) == 0) */
/*     { */
      
/*     } */
/* } */

/* void */
/* task_mutex_unlock(task_mutex_t mutex, processor_t proc) */
/* { */

/* } */


void
task_mutex_lock(volatile task_mutex_t mutex, volatile processor_t proc)
{
  __atomic_add_fetch(&mutex->count, 1, __ATOMIC_SEQ_CST);

  while (!__sync_bool_compare_and_swap(&mutex->owner, NULL, proc))
    {
      // if the owner is already set then we add to the wait list
      // and yield to the executor.

      bool success = queue_impl_enqueue(mutex->wait_queue, proc);
      assert(success);

      task_set_state(proc->task, TASK_TRANSITION_TO_WAITING);
      yield_to_executor(proc);
    }
}

void
task_mutex_unlock(volatile task_mutex_t mutex, volatile processor_t proc)
{
  assert(mutex->owner == proc);
  assert(mutex->count > 0);
  
  /* __atomic_clear(&mutex->owner, __ATOMIC_SEQ_CST); */
  /* mutex->owner = NULL; */
  __atomic_store_8(&mutex->owner, 0, __ATOMIC_SEQ_CST);

  if(  __atomic_sub_fetch(&mutex->count, 1, __ATOMIC_SEQ_CST) > 0)
    {
      processor_t other_proc = NULL;
      while(!queue_impl_dequeue(mutex->wait_queue, (void**)&other_proc));
      // If there's someone in the loop, spin to dequeue them from the
      // wait-queue.
      //
      // Spinning is OK here because we only have to wait between when they
      // increased the counter and when the enqueue themselves which
      // is a finite amount of time.
      proc_wake(other_proc);
    }
}
