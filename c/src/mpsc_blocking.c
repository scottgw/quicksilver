#include <assert.h>
#include <stdlib.h>

#include "libqs/clos_q.h"
#include "libqs/mpsc_impl.h"
#include "libqs/private_queue.h"
#include "libqs/processor.h"
#include "libqs/task.h"
#include "libqs/types.h"

struct closq_node
{ 
    struct closq_node* volatile  next; 
    void*                        state; 

}; 

typedef struct closq_node closq_node_t;

struct closq
{ 
    closq_node_t* volatile  head; 
    closq_node_t*           tail; 
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
 
/* struct closq */
/* { */
/*   volatile priv_queue_t  head; */
/*   priv_queue_t           tail; */
/*   struct priv_queue      stub; */
/* }; */

/* typedef struct closq closq_t; */

/* static */
/* void */
/* closq_create(closq_t* self)  */
/* {  */
/*   self->head = &self->stub; */
/*   self->tail = &self->stub; */
/*   self->stub.next = 0; */
/* }  */

/* static */
/* void */
/* closq_push(closq_t* self, priv_queue_t n) */
/* { */
/*   n->next = 0; */
/*   priv_queue_t prev; */
/*   __atomic_exchange(&self->head, &n, &prev,  __ATOMIC_SEQ_CST); */
/*   //(*) */
/*   prev->next = n; */
/* } */

/* static */
/* priv_queue_t */
/* closq_pop(closq_t* self) */
/* { */
/*     priv_queue_t tail = self->tail; */
/*     priv_queue_t next = tail->next; */
/*     if (tail == &self->stub) */
/*     { */
/*         if (0 == next) */
/*             return 0; */
/*         self->tail = next; */
/*         tail = next; */
/*         next = next->next; */
/*     } */
/*     if (next) */
/*     { */
/*         self->tail = next; */
/*         return tail; */
/*     } */
/*     priv_queue_t head = self->head; */
/*     if (tail != head) */
/*         return 0; */
/*     closq_push(self, &self->stub); */
/*     next = tail->next; */
/*     if (next) */
/*     { */
/*         self->tail = next; */
/*         return tail; */
/*     } */
/*     return 0; */
/* } */


struct qo_queue
{
  volatile uint32_t count;
  volatile processor_t waiter;
  
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
qo_q_enqueue_wait(qo_queue_t q, void *data, processor_t proc)
{
  assert (data != NULL);

  int n = __sync_fetch_and_add(&q->count, 1);

  if (n == -1)
    {
      closq_node_t *node = malloc (sizeof(*node));
      node->state = data;
      closq_push(q->impl, node);

      while (q->waiter == NULL);
      proc_wake (q->waiter, proc->executor);
    }
  else if (n >= q->max)
    {
      mpscq_push(&q->producers, proc);
      task_set_state(proc->task, TASK_TRANSITION_TO_WAITING);
      proc_yield_to_executor(proc);

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
qo_q_dequeue_wait(qo_queue_t q, void **data, processor_t proc)
{
  int n = __sync_fetch_and_sub(&q->count, 1);
  closq_node_t *node;
  if (n > q->max)
    {
      while ((node = closq_pop(q->impl)) == NULL);
      *data = node->state;
      assert (*data != NULL);

      processor_t producer;
      while ((producer = mpscq_pop(&q->producers)) == NULL);
      proc_wake (producer, proc->executor);
    }
  else if (n == 0)
    {
      q->waiter = proc;
      task_set_state(proc->task, TASK_TRANSITION_TO_WAITING);
      proc_yield_to_executor(proc);

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
