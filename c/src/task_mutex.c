#include <assert.h>
#include <stdlib.h>

#include "libqs/debug_log.h"
#include "libqs/processor.h"
#include "libqs/mpscq.h"
#include "libqs/queue_impl.h"
#include "libqs/sched_task.h"
#include "libqs/sync_ops.h"
#include "libqs/task.h"
#include "libqs/task_mutex.h"
#include "libqs/types.h"

#define INIT_WAIT_QUEUE_SIZE 16000

struct task_mutex 
{
  volatile uint32_t count;
  volatile sched_task_t owner;
  /* struct mpscq_node_t stub; */

  mpscq_t wait_queue;
};

task_mutex_t
task_mutex_new()
{
  task_mutex_t mutex = (task_mutex_t)malloc(sizeof(struct task_mutex));
  mutex->count = 0;
  mutex->owner = NULL;

  mpscq_create(&mutex->wait_queue, NULL); // mutex->stub);

  return mutex;
}

void
task_mutex_free(task_mutex_t mutex)
{
  free (mutex);
}

sched_task_t
task_mutex_owner(task_mutex_t mutex)
{
  return mutex->owner;
}

void
task_mutex_lock(task_mutex_t mutex, volatile sched_task_t stask)
{
  DEBUG_LOG(2, "%p requests mutex %p\n", proc, mutex);

  if (__atomic_fetch_add(&mutex->count, 1, __ATOMIC_SEQ_CST) > 0)
    {
      // if the owner is already set then we add to the wait list
      // and yield to the executor.
      DEBUG_LOG(2, "%p fails to attain %p\n", proc, mutex);

      /* mpscq_node_t* node = malloc (sizeof(*node)); */
      /* node->state = proc; */
      /* node->state = proc; */
      /* proc->node.state = proc; */

      task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);

      mpscq_push(&mutex->wait_queue, stask);

      stask_yield_to_executor(stask);

      DEBUG_LOG(2, "%p retries mutex %p\n", stask, mutex);
    }

  mutex->owner = stask;
  DEBUG_LOG(2, "%p attains mutex %p\n", stask, mutex);
}

void
task_mutex_unlock(volatile task_mutex_t mutex, sched_task_t stask)
{
  assert(mutex->owner == stask);
  assert(mutex->count > 0);

  DEBUG_LOG(2, "%p unlocks mutex %p\n", stask, mutex);

  if (__atomic_fetch_sub(&mutex->count, 1, __ATOMIC_SEQ_CST) > 1)
    {
      DEBUG_LOG(2, "%p found another waiting on mutex %p\n", stask, mutex);
      
      /* mpscq_node_t* node; */
      volatile sched_task_t other_stask;

      do
        {
          other_stask = mpscq_pop(&mutex->wait_queue);
        } while(other_stask == 0);

      DEBUG_LOG(2, "%p is awoken out of mutex %p\n", other_stask, mutex);
      // If there's someone in the loop, spin to dequeue them from the
      // wait-queue.
      //
      // Spinning is OK here because we only have to wait between when they
      // increased the counter and when the enqueue themselves which
      // is a finite amount of time.
      stask_wake(other_stask, stask->executor);
    }
}
