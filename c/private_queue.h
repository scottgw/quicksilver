#ifndef __PRIVATE_QUEUE_H
#define __PRIVATE_QUEUE_H
typedef struct node_ {
  void *data;
  struct node_ *next;
} node;

typedef struct {
  node *oldest;
  node *newest;
} private_queue;

private_queue*
pq_create();

void
pq_free(private_queue *queue);

int
pq_enqueue (private_queue *queue, void *e);
int
pq_dequeue (private_queue *queue, void **data);
#endif
