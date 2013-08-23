#include <assert.h>
#include <stdlib.h>

#include "libqs/mpsc_blocking.h"
#include "libqs/mpsc_impl.h"
#include "libqs/private_queue.h"
#include "libqs/processor.h"
#include "libqs/task.h"
#include "libqs/types.h"

struct closq_node
{
  struct closq_node* volatile next;
  void*                       state;
};

typedef struct closq_node closq_node_t;

struct closq
{
  closq_node_t* volatile head;
  closq_node_t*          tail;
};

typedef struct closq closq_t;

void closq_create(closq_t* self, closq_node_t* stub)
{
  stub->next = 0;
  self->head = stub;
  self->tail = stub;
}

void closq_push(closq_t* self, closq_node_t* n)
{
  n->next = 0;
  closq_node_t* prev;
  __atomic_exchange(&self->head, &n, &prev,  __ATOMIC_SEQ_CST);
  prev->next = n; // serialization-point wrt consumer, release
}

closq_node_t* closq_pop(closq_t* self)
{
  closq_node_t* tail = self->tail;
  closq_node_t* next = tail->next; // serialization-point wrt producers, acquire
  if (next)
    {
      self->tail = next;
      tail->state = next->state;
      return tail;
    }
  return 0;
}

struct qo_queue
{
  volatile uint32_t count;
  volatile sched_task_t waiter;

  mpscq_t producers;

  uint64_t max;

  closq_t *impl;
  closq_node_t stub;
};

qo_queue_t
qo_q_new(uint32_t size)
{
  qo_queue_t q = (qo_queue_t)malloc(sizeof(*q));

  q->count  = 0;
  q->max    = size;

  mpscq_create (&q->producers, NULL);

  q->impl   = malloc(sizeof(closq_t));
  closq_node_t *node = malloc (sizeof(*node));
  closq_create (q->impl, node);

  return q;
}

void
qo_q_enqueue_wait(qo_queue_t q, void *data, sched_task_t stask)
{
  assert (data != NULL);

  int n = __sync_fetch_and_add(&q->count, 1);

  if (n == -1)
    {
      closq_node_t *node = malloc (sizeof(*node));
      node->state = data;
      closq_push(q->impl, node);

      while (q->waiter == NULL);
      stask_wake (q->waiter, stask->executor);
    }
  else if (n >= q->max)
    {
      mpscq_push(&q->producers, stask);
      task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);
      stask_yield_to_executor(stask);

      closq_node_t *node = malloc (sizeof(*node));
      node->state = data;
      closq_push(q->impl, node);
    }
  else
    {
      closq_node_t *node = malloc (sizeof(*node));
      node->state = data;
      closq_push(q->impl, node);
    }
}

void
qo_q_dequeue_wait(qo_queue_t q, void **data, sched_task_t stask)
{
  int n = __sync_fetch_and_sub(&q->count, 1);
  closq_node_t *node;
  if (n > q->max)
    {
      while ((node = closq_pop(q->impl)) == NULL);
      *data = node->state;
      assert (*data != NULL);

      sched_task_t producer;
      while ((producer = mpscq_pop(&q->producers)) == NULL);
      stask_wake (producer, stask->executor);
    }
  else if (n == 0)
    {
      task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);
      q->waiter = stask;
      stask_yield_to_executor(stask);

      node = closq_pop(q->impl);
      *data = node->state;
      assert (*data != NULL);
    }
  else
    {
      while ((node = closq_pop(q->impl)) == NULL);
      *data = node->state;
      assert (*data != NULL);
    }

  free(node);
}

void
qo_q_free(qo_queue_t q)
{
  
}
