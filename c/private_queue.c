#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "private_queue.h"

private_queue*
pq_create()
{
  private_queue *new_queue = (private_queue*) malloc(sizeof(private_queue));
  node* new_node = (node*) malloc(sizeof(node));
  new_queue->oldest = new_node;
  new_queue->newest = new_node;
  return new_queue;
}

void
pq_free(private_queue *queue)
{
  node* current = queue->oldest;
  while (current != NULL) 
    {
      node* next = current->next;
      free(current);
      current = next;
    }
  free(queue);
}


int
pq_enqueue (private_queue *queue, void *e)
{
  node *new_node = (node*) malloc(sizeof(node));

  new_node->data = e;
  new_node->next = NULL;

  // On modern x86 this should work, as only stores can be reordered after
  // independent loads.
  {
    node * volatile *vol_node;
    vol_node = &queue->newest->next;
    *vol_node = new_node;
  }

  // We don't have to worry about the queue->newest pointer in the
  // single-producer as this assumption means no other threads
  // would modify it.
  queue->newest = new_node;
  return 0;
}

int
pq_dequeue (private_queue *queue, void **data)
{
  node * old_node;
  node * volatile *vol_node;

  vol_node = &(queue->oldest->next);
  old_node = *vol_node;

  if (old_node == NULL)
    return 0;

  *data = old_node->data;
  free (queue->oldest);
  queue->oldest = old_node;

  return 1;
}
