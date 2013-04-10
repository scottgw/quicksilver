#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>

#include "conc_queue.h"

#define CAS(ptr1, ptr2, val)                                            \
  __atomic_compare_exchange((ptr1),(ptr2),(val),                        \
                            false,                                      \
                            __ATOMIC_RELAXED, __ATOMIC_RELAXED)

/* #define CAS(ptr1, ptr2, val)                                            \ */
/*   __sync_bool_compare_and_swap((ptr1),(ptr),(val)) */


union pointer_int
{
  struct pointer p;
  unsigned __int128 i;
};

inline
static
unsigned __int128
ptr_to_128(struct pointer p)
{
  /* union pointer_int ptr_union ; */
  /* ptr_union.p = p;  */
  /* return ptr_union.i; */
  /* unsigned __int128 x = (__int128)((void*)p.ptr); */
  /* unsigned __int128 y = (__int128)p.count; */
  /* return (x << 64) | y; */
  return (*(unsigned __int128*)(&p));
}

static
bool
pointer_eq (struct pointer p1, struct pointer p2)
{
  return p1.ptr == p2.ptr && p1.count == p2.count;
}

static
node_t
node_make()
{
  return (node_t) malloc(sizeof(struct node));
}

cqueue_t
cq_make()
{
  cqueue_t q = (cqueue_t) malloc(sizeof(struct queue));
  node_t node = node_make();

  node->next.ptr = NULL;
  q->head.ptr = node;
  q->tail.ptr = node;

  return q;
}

void
cq_free(cqueue_t q)
{
  node_t n = q->head.ptr;
  for ( ; n != NULL; n = n->next.ptr)
    {
      free(n);
    }
  free(q);
}


void
cq_enqueue(cqueue_t q, void* value)
{
  node_t node = node_make();
  struct pointer tail;
  node->value = value;
  node->next.ptr = NULL;

  while(1)
    {    
      tail = q->tail;
      struct pointer next = tail.ptr->next;
      if (pointer_eq(tail, q->tail))
        {
          if (next.ptr == NULL)
            {
              struct pointer new_ptr = { node, next.count + 1 };
              if (CAS(&tail.ptr->next, &next, &new_ptr))
                {
                  break;
                }
            }
          else
            {
              struct pointer new_ptr = { next.ptr, tail.count + 1 };
              CAS(&q->tail, &tail, &new_ptr);
            }
        }
    }
  struct pointer new_ptr = { node, tail.count + 1 };
  CAS(&q->tail, &tail, &new_ptr);
}  

bool
cq_dequeue(cqueue_t q, void **result)
{
  struct pointer head;
  while(1)
    {
      head = q->head;
      struct pointer tail = q->tail;
      struct pointer next = head.ptr->next;
      if (pointer_eq(head, q->head))
        {
          if (head.ptr == tail.ptr)
            {
              if (next.ptr == NULL)
                {
                  return false;
                }
              struct pointer new_ptr = {next.ptr, tail.count + 1};
              CAS(&q->tail, &tail, &new_ptr);
            }
          else
            {
              *result = next.ptr->value;
              struct pointer new_ptr = {next.ptr, head.count + 1};
              if (CAS(&q->head, &head, &new_ptr))
                {
                  break;
                }
            }
        }
    }
  free(head.ptr);
  return true;
}
