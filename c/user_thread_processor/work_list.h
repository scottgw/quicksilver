#ifndef __WORK_LIST_H_
#define __WORK_LIST_H_

#include "types.h"

list_t
list_make();

void*
list_take(list_t);

void
list_add(list_t, void*);

void
list_foreach(list_t, void (*)(void*, void*), void*);

int
list_size(list_t);

void
list_free(list_t);

#endif // __WORK_LIST_H_
