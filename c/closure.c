#include <stdlib.h>

#include "closure.h"

struct closure
{
  void (*func)(void*);
  void *ptr;
};

closure_t
closure_new(void (*func)(void*), void* ptr)
{
  closure_t clos = (closure_t) malloc(sizeof(struct closure));
  clos->func = func;
  clos->ptr = ptr;
  return clos;
}

void
closure_apply(closure_t clos)
{
  clos->func(clos->ptr);
}
