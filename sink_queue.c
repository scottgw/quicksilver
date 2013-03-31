#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "sink_queue.h"

sink_queue*
sinkq_create()
{
  sink_queue *new_queue = (sink_queue*) malloc(sizeof(sink_queue));
  sink_node* new_sink_node = (sink_node*) malloc(sizeof(sink_node));
  new_queue->oldest = new_sink_node;
  new_queue->newest = new_sink_node;
  return new_queue;
}

void
sinkq_free(sink_queue *queue)
{
  sink_node* current = queue->oldest;
  while (current != NULL) 
    {
      sink_node* next = current->next;
      free(current);
      current = next;
    }
  free(queue);
}


int
sinkq_enqueue (sink_queue *queue, void *e)
{
  sink_node *new_sink_node = (sink_node*) malloc(sizeof(sink_node));
  sink_node *prev_newest;
  new_sink_node->data = e;
  new_sink_node->next = NULL;

  //  prev_newest = xchg (&queue->newest, new_sink_node);
  
  prev_newest->next = new_sink_node;

  return 0;
}

int
sinkq_dequeue (sink_queue *queue, void **data)
{
  sink_node * old_sink_node;
  sink_node * volatile *vol_sink_node;

  vol_sink_node = &(queue->oldest->next);
  old_sink_node = *vol_sink_node;

  if (old_sink_node == NULL)
    return 0;

  *data = old_sink_node->data;
  free (queue->oldest);
  queue->oldest = old_sink_node;

  return 1;
}
