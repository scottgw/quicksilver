#include "internal/mpscq.h"

void mpscq_create(mpscq_t* self, mpscq_node_t* stub) 
{ 
  /* stub->next = 0; */
  /* self->head = stub; */
  /* self->tail = stub; */
  self->head = &self->stub;
  self->tail = &self->stub;
  self->stub.next = 0;
} 

void mpscq_push(mpscq_t* self, sched_task_t n)
{
  n->next = 0;
  sched_task_t prev;
  __atomic_exchange(&self->head, &n, &prev,  __ATOMIC_SEQ_CST);
  //(*)
  prev->next = n;
}

sched_task_t mpscq_pop(mpscq_t* self)
{
    sched_task_t tail = self->tail;
    sched_task_t next = tail->next;
    if (tail == &self->stub)
    {
        if (0 == next)
            return 0;
        self->tail = next;
        tail = next;
        next = next->next;
    }
    if (next)
    {
        self->tail = next;
        return tail;
    }
    sched_task_t head = self->head;
    if (tail != head)
        return 0;
    mpscq_push(self, &self->stub);
    next = tail->next;
    if (next)
    {
        self->tail = next;
        return tail;
    }
    return 0;
}

/* mpscq_node_t* mpscq_pop(mpscq_t* self) */
/* { */
/*   mpscq_node_t* tail = self->tail; */
/*   mpscq_node_t* next = tail->next; // serialization-point wrt producers, acquire */
/*   if (next) */
/*     { */
/*       self->tail = next; */
/*       tail->state = next->state; */
/*       return tail; */
/*     } */
/*   return 0; */
/* } */
