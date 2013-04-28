#include <stdlib.h>

#include "closure.h"

struct closure
{
  ffi_cif *cif;
  void *fn;
  int argc;
  void **args;
};

closure_t
closure_new(ffi_cif *cif, void *fn, int argc, void **args)
{
  closure_t clos = (closure_t) malloc(sizeof(struct closure));
  clos->cif = cif;
  clos->fn = fn;
  clos->argc = argc;
  clos->args = args;
  return clos;
}

void
closure_apply(closure_t clos, void* res)
{
  ffi_call(clos->cif, clos->fn, res, clos->args);
  free(clos->cif->arg_types);
  free(clos->cif);

  for (int i = 0; i < clos->argc; i++)
    {
      free(clos->args[i]);
    }

  free(clos->args);
  free(clos);
}
