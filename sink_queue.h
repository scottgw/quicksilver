#ifndef __SINK_QUEUE_H
#define __SINK_QUEUE_H

typedef struct sink_node_ {
  void *data;
  struct sink_node_ *volatile next;
} sink_node;

typedef struct {
  sink_node *oldest;
  sink_node *volatile newest;
} sink_queue;

sink_queue*
sinkq_create();

void
sinkq_free(sink_queue *queue);

int
sinkq_enqueue (sink_queue *queue, void *e);

int
sinkq_dequeue (sink_queue *queue, void **data);

#endif
