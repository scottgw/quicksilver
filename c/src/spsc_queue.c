#include <assert.h>
#include <stdlib.h>

#include "ck_ring.h"

#include "libqs/spsc_queue.h"
#include "libqs/bounded_queue.h"
#include "libqs/debug_log.h"
#include "libqs/processor.h"
#include "libqs/queue_impl.h"
#include "libqs/task.h"
#include "libqs/task_mutex.h"
#include "libqs/task_condition.h"

#define SPSC_CUSTOM

struct spsc_queue
{
  uint32_t max;
  volatile uint32_t count;

  volatile processor_t waiter;

#ifdef SPSC_CUSTOM
  uint64_t idx;
  ck_ring_t impl;
#else
  bounded_queue_t impl;
#endif

};

spsc_queue_t
spsc_new(uint32_t size)
{
  spsc_queue_t q = (spsc_queue_t)malloc(sizeof(struct spsc_queue));

  q->count  = 0;
  q->max    = size;
  q->waiter = NULL;
#ifdef SPSC_CUSTOM
  void **buffer = malloc(size * sizeof(void*));
  ck_ring_init (&q->impl, buffer, size);
#else
  q->impl   = bqueue_new(size);
#endif

  return q;
}

void
spsc_free(spsc_queue_t q)
{
#ifdef SPSC_CUSTOM
  free(q->impl.ring);
#else
  bqueue_free(q->impl);
#endif
  free(q);
}

#ifdef SPSC_CUSTOM
void
spsc_enqueue_wait(spsc_queue_t q, void *data, processor_t proc)
{
  int n = __sync_fetch_and_add(&q->count, 1);
  q->idx++;

  if (n == -1)
    {
      ck_ring_enqueue_spsc(&q->impl, data);

      while (q->waiter == NULL);

      processor_t waiter = q->waiter;
      q->waiter = NULL;
      proc_wake(waiter, proc->executor);
    }
  else if (n == q->max)
    {
      q->waiter = proc;
      task_set_state(proc->task, TASK_TRANSITION_TO_WAITING);
      proc_yield_to_executor(proc);

      ck_ring_enqueue_spsc(&q->impl, data);
   }
  else
    {
      while(!ck_ring_enqueue_spsc(&q->impl, data));
      /* bool success = queue_impl_enqueue(q->impl, data); */
      /* assert(success); */
    }
}


void
spsc_dequeue_wait(spsc_queue_t q, void **data, processor_t proc)
{
  int n = __sync_fetch_and_sub(&q->count, 1);
  if (n == q->max + 1)
    {
      bool success = ck_ring_dequeue_spsc(&q->impl, data);
      assert(success);

      while (q->waiter == NULL);

      // At this point no other processors can be in the queue.
      processor_t waiter = q->waiter;
      q->waiter = NULL;
      proc_wake(waiter, proc->executor);
    }
  else if (n == 0)
    {
      q->waiter = proc;
      task_set_state(proc->task, TASK_TRANSITION_TO_WAITING);
      proc_yield_to_executor(proc);

      bool success = ck_ring_dequeue_spsc(&q->impl, data);
      assert(success);
    }
  else
    {
      while(!ck_ring_dequeue_spsc(&q->impl, data))
        {
          proc->task->state = TASK_TRANSITION_TO_RUNNABLE;
          proc_yield_to_executor(proc);
        }
    }
  
}
#else
void
spsc_enqueue_wait(spsc_queue_t q, void *data, processor_t proc)
{
  bqueue_enqueue_wait(q->impl, data, proc);
}

void
spsc_dequeue_wait(spsc_queue_t q, void **data, processor_t proc)
{
  bqueue_dequeue_wait(q->impl, data, proc);
}
#endif
