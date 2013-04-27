#include <stdlib.h>

#include "closure.h"

struct closure
{
  ffi_cif *cif;
  void *fn;
  void **args;
};

closure_t
closure_new(ffi_cif *cif, void *fn, void **args)
{
  closure_t clos = (closure_t) malloc(sizeof(struct closure));
  clos->cif = cif;
  clos->fn = fn;
  clos->args = args;
  return clos;
}

void
closure_apply(closure_t clos)
{
  ffi_call(clos->cif, clos->fn, NULL, clos->args);
  free(clos->cif->arg_types);
  free(clos->cif);
  free(clos->args);
}
