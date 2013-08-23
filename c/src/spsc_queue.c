#include <assert.h>
#include <stdlib.h>
#include <ck_ring.h>

#include "internal/spsc_queue.h"
#include "internal/bounded_queue.h"
#include "internal/debug_log.h"
#include "internal/queue_impl.h"
#include "internal/sched_task.h"
#include "internal/task.h"
#include "internal/task_mutex.h"
#include "internal/task_condition.h"

struct spsc_queue
{
  uint32_t max;
  volatile uint32_t count;

  volatile sched_task_t waiter;


  ck_ring_t impl;
};

spsc_queue_t
spsc_new(uint32_t size)
{
  spsc_queue_t q = (spsc_queue_t)malloc(sizeof(struct spsc_queue));

  q->count  = 0;
  q->max    = size;
  q->waiter = NULL;

  void **buffer = malloc(size * sizeof(void*));
  ck_ring_init (&q->impl, buffer, size);

  return q;
}

void
spsc_free(spsc_queue_t q)
{
  free(q->impl.ring);
  free(q);
}

void
spsc_enqueue_wait(spsc_queue_t q, void *data, sched_task_t stask)
{
  int n = __sync_fetch_and_add(&q->count, 1);

  if (n == -1)
    {
      ck_ring_enqueue_spsc(&q->impl, data);

      while (q->waiter == NULL);

      sched_task_t waiter = q->waiter;
      q->waiter = NULL;
      stask_wake(waiter, stask->executor);
    }
  else if (n == q->max)
    {
      q->waiter = stask;
      task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);
      stask_yield_to_executor(stask);

      ck_ring_enqueue_spsc(&q->impl, data);
   }
  else
    {
      while(!ck_ring_enqueue_spsc(&q->impl, data))
        {
          stask->task->state = TASK_TRANSITION_TO_RUNNABLE;
          stask_yield_to_executor(stask);
        }
    }
}


void
spsc_dequeue_wait(spsc_queue_t q, void **data, sched_task_t stask)
{
  int n = __sync_fetch_and_sub(&q->count, 1);
  if (n == q->max + 1)
    {
      bool success = ck_ring_dequeue_spsc(&q->impl, data);
      assert(success);

      while (q->waiter == NULL);

      // At this point no other staskessors can be in the queue.
      sched_task_t waiter = q->waiter;
      q->waiter = NULL;
      stask_wake(waiter, stask->executor);
    }
  else if (n == 0)
    {
      q->waiter = stask;
      task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);
      stask_yield_to_executor(stask);

      bool success = ck_ring_dequeue_spsc(&q->impl, data);
      assert(success);
    }
  else
    {
      while(!ck_ring_dequeue_spsc(&q->impl, data))
        {
          stask->task->state = TASK_TRANSITION_TO_RUNNABLE;
          stask_yield_to_executor(stask);
        }
    }
  
}
