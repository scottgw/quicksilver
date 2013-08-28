#ifndef __MPSC_IMPL_H
#define __MPSC_IMPL_H


#include "../libqs/types.h"
#include "sched_task.h"

#ifdef __cplusplus
extern "C" {
#endif

/* struct mpscq_node */
/* { */
/*   struct mpscq_node* volatile next; */
/*   /\* void*                       state;  *\/ */
/* }; */
/* typedef struct mpscq_node mpscq_node_t; */

struct mpscq
{
  sched_task_t volatile  head;
  sched_task_t           tail;
  struct sched_task      stub;
};
typedef struct mpscq mpscq_t;


void mpscq_create(mpscq_t* self, mpscq_node_t* stub);
void mpscq_push(mpscq_t* self, sched_task_t n);
sched_task_t mpscq_pop(mpscq_t* self);


#ifdef __cplusplus
}
#endif
#endif // __MPSC_IMPL_H
