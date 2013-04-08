#include <assert.h>
#include <stdlib.h>

#include "work_list.h"

#define LIST_SIZE 1024

work_list_t
make_work_list()
{
  work_list_t list = (work_list_t)malloc(sizeof(struct work_list));
  list->data = malloc(LIST_SIZE*sizeof(void*));
  list->start = 0;
  list->end = 0;
  list->size = 0;
  return list;
}

void*
take_work_item(work_list_t list)
{
  void* data;
  assert (list->size != 0);
  data = list->data[list->start];
  list->start = (list->start + 1) % LIST_SIZE;
  list->size--;
  return data;
}

void
add_work_item(work_list_t list, void* data)
{
  *(list->data + list->end) = data;
  list->size++;
  list->end = (list->end + 1) % LIST_SIZE;
}

int
work_list_size(work_list_t list)
{
  assert (list != NULL);
  return list->size;
}


void
free_work_list(work_list_t list)
{
  free(list->data);
  free(list);
}
