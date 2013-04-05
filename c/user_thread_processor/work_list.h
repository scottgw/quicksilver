#ifndef __WORK_LIST_H_
#define __WORK_LIST_H_

#include <glib.h>

typedef struct
{
  GSequence* seq;
} work_list_t;


work_list_t
make_work_list();

void*
take_work_item(work_list_t);

void
add_work_item(work_list_t, void*);

int
work_list_size(work_list_t);

void
free_work_list(work_list_t);

#endif // __WORK_LIST_H_
