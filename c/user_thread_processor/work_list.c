#include <assert.h>
#include <stdlib.h>

#include "work_list.h"

#define LIST_SIZE 1024

list_t
list_make()
{
  list_t list = (list_t)malloc(sizeof(struct list));
  list->data = malloc(LIST_SIZE*sizeof(void*));
  list->start = 0;
  list->end = 0;
  list->size = 0;
  return list;
}

void*
list_take(list_t list)
{
  void* data;
  assert (list->size != 0);
  data = list->data[list->start];
  list->start = (list->start + 1) % LIST_SIZE;
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
  list->end = (list->end + 1) % LIST_SIZE;
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
