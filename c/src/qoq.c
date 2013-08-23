#include <assert.h>
#include <stdlib.h>

#include "libqs/private_queue.h"
#include "libqs/processor.h"
#include "libqs/types.h"

#include "internal/qoq.h"
#include "internal/mpscq.h"
#include "internal/task.h"


struct mpsc_node
{
  struct mpsc_node* volatile next;
  void*                       state;
};

typedef struct mpsc_node mpsc_node_t;

struct mpsc
{
  mpsc_node_t* volatile head;
  mpsc_node_t*          tail;
};

typedef struct mpsc mpsc_t;

static
void
mpsc_create(mpsc_t* self, mpsc_node_t* stub)
{
  stub->next = 0;
  self->head = stub;
  self->tail = stub;
}

static
void
mpsc_push(mpsc_t* self, mpsc_node_t* n)
{
  n->next = 0;
  mpsc_node_t* prev;
  __atomic_exchange(&self->head, &n, &prev,  __ATOMIC_SEQ_CST);
  prev->next = n; // serialization-point wrt consumer, release
}

static
mpsc_node_t*
mpsc_pop(mpsc_t* self)
{
  mpsc_node_t* tail = self->tail;
  mpsc_node_t* next = tail->next; // serialization-point wrt producers, acquire
  if (next)
    {
      self->tail = next;
      tail->state = next->state;
      return tail;
    }
  return 0;
}

struct qoq
{
  volatile uint32_t count;
  volatile sched_task_t waiter;

  mpscq_t producers;

  uint64_t max;

  mpsc_t *impl;
  mpsc_node_t stub;
};

qoq_t
qoq_new(uint32_t size)
{
  qoq_t q = (qoq_t)malloc(sizeof(*q));

  q->count  = 0;
  q->max    = size;

  mpscq_create (&q->producers, NULL);

  q->impl   = malloc(sizeof(mpsc_t));
  mpsc_node_t *node = malloc (sizeof(*node));
  mpsc_create (q->impl, node);

  return q;
}

void
qoq_enqueue_wait(qoq_t q, void *data, sched_task_t stask)
{
  assert (data != NULL);

  int n = __sync_fetch_and_add(&q->count, 1);

  if (n == -1)
    {
      mpsc_node_t *node = malloc (sizeof(*node));
      node->state = data;
      mpsc_push(q->impl, node);

      while (q->waiter == NULL);
      stask_wake (q->waiter, stask->executor);
    }
  else if (n >= q->max)
    {
      mpscq_push(&q->producers, stask);
      task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);
      stask_yield_to_executor(stask);

      mpsc_node_t *node = malloc (sizeof(*node));
      node->state = data;
      mpsc_push(q->impl, node);
    }
  else
    {
      mpsc_node_t *node = malloc (sizeof(*node));
      node->state = data;
      mpsc_push(q->impl, node);
    }
}

void
qoq_dequeue_wait(qoq_t q, void **data, sched_task_t stask)
{
  int n = __sync_fetch_and_sub(&q->count, 1);
  mpsc_node_t *node;
  if (n > q->max)
    {
      while ((node = mpsc_pop(q->impl)) == NULL);
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

      node = mpsc_pop(q->impl);
      *data = node->state;
      assert (*data != NULL);
    }
  else
    {
      while ((node = mpsc_pop(q->impl)) == NULL);
      *data = node->state;
      assert (*data != NULL);
    }

  free(node);
}

void
qoq_free(qoq_t q)
{
  
}
