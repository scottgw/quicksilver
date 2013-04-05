#include <glib.h>

#include "work_list.h"

work_list_t
make_work_list()
{
  work_list_t list;
  list.seq = g_sequence_new(NULL);
  return list;
}

void*
take_work_item(work_list_t list)
{
  GSequence* seq = list.seq;
  GSequenceIter* it = g_sequence_get_begin_iter(seq);
  gpointer data = g_sequence_get(it);
  g_sequence_remove(it);
  return data;
}

void
add_work_item(work_list_t list, void* data)
{
  g_sequence_append(list.seq, data);
}

int
work_list_size(work_list_t list)
{
  return g_sequence_get_length(list.seq);
}


void
free_work_list(work_list_t list)
{
  g_sequence_free(list.seq);
}
