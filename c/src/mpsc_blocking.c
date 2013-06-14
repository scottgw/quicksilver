#include <assert.h>
#include <stdlib.h>

#include "libqs/processor.h"
#include "libqs/task.h"
#include "libqs/types.h"

struct mpsc_queue
{
  volatile uint32_t count;
  volatile processor_t waiter;
  uint64_t max;

  uint64_t idx;  
  mpscq_t *impl;
  mpscq_node_t *entries;
};


typedef struct mpsc_queue* mpsc_queue_t;


mpsc_queue_t
mpsc_new(uint32_t size)
{
  mpsc_queue_t q = (mpsc_queue_t)malloc(sizeof(struct mpsc_queue));

  q->count  = 0;
  q->max    = size;
  q->waiter = NULL;

  q->idx    = 0;
  q->impl   = malloc(sizeof(mpscq_t));
  /* q->entries = malloc((size+2) * sizeof(mpscq_node_t)); */
  mpscq_create (q->impl, q->entries);

  return q;
}


void
mpsc_enqueue_wait(mpsc_queue_t q, void *data, processor_t proc)
{
  int n = __sync_fetch_and_add(&q->count, 1);
  /* q->idx++; */
  /* mpscq_node_t *entry = q->entries + (q->idx % (q->max + 2)); */
  mpscq_node_t *entry = malloc (sizeof(mpscq_node_t));
  entry->state = data;

  if (n == -1)
    {
      mpscq_push(q->impl, entry);

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
      mpscq_push(q->impl, entry);
   }
  else
    {
      // fixme: can't just enqueue here
      mpscq_push(q->impl, entry);
    }
}


void
mpsc_dequeue_wait(mpsc_queue_t q, void **data, processor_t proc)
{
  int n = __sync_fetch_and_sub(&q->count, 1);
  if (n == q->max + 1)
    {
      bool success = mpscq_pop(q->impl, data);
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

      bool success = mpscq_pop(q->impl, data);
      assert(success);
    }
  else
    {
      while(!mpscq_pop(q->impl, data));
      /* bool success = queue_impl_dequeue(q->impl, data); */
      /* assert(success); */
    }
  
}
