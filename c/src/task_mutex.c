#include <assert.h>
#include <stdlib.h>

#include "internal/debug_log.h"
#include "internal/mpscq.h"
#include "internal/sched_task.h"
#include "internal/task.h"
#include "internal/task_mutex.h"


#define INIT_WAIT_QUEUE_SIZE 16000

struct task_mutex 
{
  uint32_t count;
  sched_task_t owner;
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
  return NULL; //  mutex->owner;
}

void
task_mutex_lock(task_mutex_t mutex, volatile sched_task_t stask)
{
  if (__atomic_fetch_add(&mutex->count, 1, __ATOMIC_SEQ_CST) > 0)
    {
      task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);

      mpscq_push(&mutex->wait_queue, stask);

      stask_yield_to_executor(stask);
    }

  mutex->owner = stask;
}

void
task_mutex_unlock(volatile task_mutex_t mutex, sched_task_t stask)
{
  assert(mutex->owner == stask);
  assert(mutex->count > 0);

  if (__atomic_fetch_sub(&mutex->count, 1, __ATOMIC_SEQ_CST) > 1)
    {
      sched_task_t other_stask;

      // Here we spin while we wait for the other thread to put their stask
      // in the queue. This is bounded because this stask only has to wait
      // between where the other thread did the atomic add and when it puts
      // itself in the queue.
      do
        {
          other_stask = mpscq_pop(&mutex->wait_queue);
        } while(other_stask == 0);

      stask_wake(other_stask, stask->executor);
    }
}
