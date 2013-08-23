#ifndef __MPSC_IMPL_H
#define __MPSC_IMPL_H


#include "types.h"
#include "sched_task.h"

/* struct mpscq_node */
/* { */
/*   struct mpscq_node* volatile next; */
/*   /\* void*                       state;  *\/ */
/* }; */
/* typedef struct mpscq_node mpscq_node_t; */

struct mpscq
{
  volatile sched_task_t  head;
  sched_task_t           tail;
  struct sched_task      stub;
};
typedef struct mpscq mpscq_t;


void mpscq_create(mpscq_t* self, mpscq_node_t* stub);
void mpscq_push(mpscq_t* self, sched_task_t n);
sched_task_t mpscq_pop(mpscq_t* self);
#endif // __MPSC_IMPL_H
