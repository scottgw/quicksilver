#include <assert.h>
#include <stdlib.h>

#include "libqs/spsc_queue.h"
#include "libqs/bounded_queue.h"
#include "libqs/debug_log.h"
#include "libqs/processor.h"
#include "libqs/queue_impl.h"
#include "libqs/task.h"
#include "libqs/task_mutex.h"
#include "libqs/task_condition.h"

struct spsc_queue
{
  uint32_t max;
  volatile uint32_t count;

  volatile processor_t waiter;

  bounded_queue_t impl;
};

spsc_queue_t
spsc_new(uint32_t size)
{
  spsc_queue_t q = (spsc_queue_t)malloc(sizeof(struct spsc_queue));

  q->count  = 0;
  q->max    = size;
  q->waiter = NULL;
  q->impl   = bqueue_new(size);

  return q;
}

void
spsc_free(spsc_queue_t q)
{
  bqueue_free(q->impl);
  free(q);
}


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


/* void */
/* spsc_enqueue_wait(spsc_queue_t q, void *data, processor_t proc) */
/* { */
/*   int n = __sync_fetch_and_add(&q->count, 1); */
/*   if (n == -1) */
/*     { */
/*       bool success = queue_impl_enqueue(q->impl, data); */
/*       assert(success); */

/*       while (q->waiter == NULL); */

/*       processor_t waiter = q->waiter; */
/*       q->waiter = NULL; */
/*       proc_wake(waiter); */
/*     } */
/*   else if (n == q->max) */
/*     { */
/*       q->waiter = proc; */
/*       task_set_state(proc->task, TASK_TRANSITION_TO_WAITING); */
/*       proc_yield_to_executor(proc); */

/*       bool success = queue_impl_enqueue(q->impl, data); */
/*       assert(success); */
/*     } */
/*   else */
/*     { */
/*       while(!queue_impl_enqueue(q->impl, data)); */
/*       /\* bool success = queue_impl_enqueue(q->impl, data); *\/ */
/*       /\* assert(success); *\/ */
/*     } */
/* } */


/* void */
/* spsc_dequeue_wait(spsc_queue_t q, void **data, processor_t proc) */
/* { */
/*   int n = __sync_fetch_and_sub(&q->count, 1); */
/*   if (n == q->max + 1) */
/*     { */
/*       bool success = queue_impl_dequeue(q->impl, data); */
/*       assert(success); */

/*       while (q->waiter == NULL); */

/*       // At this point no other processors can be in the queue. */
/*       processor_t waiter = q->waiter; */
/*       q->waiter = NULL; */
/*       proc_wake(waiter); */
/*     } */
/*   else if (n == 0) */
/*     { */
/*       q->waiter = proc; */
/*       task_set_state(proc->task, TASK_TRANSITION_TO_WAITING); */
/*       proc_yield_to_executor(proc); */

/*       bool success = queue_impl_dequeue(q->impl, data); */
/*       assert(success); */
/*     } */
/*   else */
/*     { */
/*       while(!queue_impl_dequeue(q->impl, data)); */
/*       /\* bool success = queue_impl_dequeue(q->impl, data); *\/ */
/*       /\* assert(success); *\/ */
/*     } */
  
/* } */

