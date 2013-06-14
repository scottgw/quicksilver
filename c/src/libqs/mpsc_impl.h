#ifndef __MPSC_IMPL_H
#define __MPSC_IMPL_H

struct mpscq_node
{
  struct mpscq_node* volatile next;
  void*                       state; 
};
typedef struct mpscq_node mpscq_node_t;

struct mpscq
{
  mpscq_node_t* volatile  head;
  mpscq_node_t*           tail;
};
typedef struct mpscq mpscq_t;


void mpscq_create(mpscq_t* self, mpscq_node_t* stub);
void mpscq_push(mpscq_t* self, mpscq_node_t* n);
mpscq_node_t* mpscq_pop(mpscq_t* self);
#endif // __MPSC_IMPL_H
