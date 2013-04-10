#ifndef _CONC_QUEUE_H
#define _CONC_QUEUE_H

#include <stdint.h>
#include <stdbool.h>

struct node;

struct pointer
{
  struct node *ptr;
  uint64_t count;
};

struct node
{
  void* value;
  struct pointer next;
};

struct queue
{
  struct pointer head;
  struct pointer tail;
};

typedef struct node * node_t;  
typedef struct queue * cqueue_t;

cqueue_t
cq_make();

void
cq_free(cqueue_t q);

void
cq_enqueue(cqueue_t q, void* value);

bool
cq_dequeue(cqueue_t q, void **result);

#endif // _CONC_QUEUE_H
