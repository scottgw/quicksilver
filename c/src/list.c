#include <assert.h>
#include <stdlib.h>

#include "list.h"

struct list
{
  void** data;
  uint32_t start;
  uint32_t end;
  uint32_t size;
  uint32_t max_size;
};

list_t
list_make(uint32_t max_size)
{
  list_t list = (list_t)malloc(sizeof(struct list));
  list->data = malloc(max_size*sizeof(void*));
  list->start = 0;
  list->end = 0;
  list->size = 0;
  list->max_size = max_size;
  return list;
}

void*
list_take(list_t list)
{
  void* data;
  assert (list->size != 0);
  data = list->data[list->start];
  list->start = (list->start + 1) % list->max_size;
  list->size--;
  return data;
}

void
list_foreach(list_t list, void (*func)(void*, void*), void* user_data)
{
  int end = list->end;
  for(int i = list->start; i < end; i++)
    {
      func(list->data[i], user_data);
    }
}

void
list_add(list_t list, void* data)
{
  *(list->data + list->end) = data;
  list->size++;
  list->end = (list->end + 1) % list->max_size;
}

int
list_size(list_t list)
{
  assert (list != NULL);
  return list->size;
}


void
list_free(list_t list)
{
  free(list->data);
  free(list);
}
