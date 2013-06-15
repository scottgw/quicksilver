#ifndef __MPSC_IMPL_H
#define __MPSC_IMPL_H


#include "types.h"
#include "processor.h"

/* struct mpscq_node */
/* { */
/*   struct mpscq_node* volatile next; */
/*   /\* void*                       state;  *\/ */
/* }; */
/* typedef struct mpscq_node mpscq_node_t; */

struct mpscq
{
  volatile processor_t  head;
  processor_t           tail;
  struct processor      stub;
};
typedef struct mpscq mpscq_t;


void mpscq_create(mpscq_t* self, mpscq_node_t* stub);
void mpscq_push(mpscq_t* self, processor_t n);
processor_t mpscq_pop(mpscq_t* self);
#endif // __MPSC_IMPL_H
