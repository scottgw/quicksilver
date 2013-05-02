#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

#include "closure.h"

struct clos_type
{
  ffi_type type;
};


closure_t
closure_new_end()
{
  closure_t clos = (closure_t) malloc(sizeof(struct closure));
  clos->is_end = true;
  return clos;
}

closure_t
closure_new(void *fn,
            clos_type_t res_type,
            int argc,
            void ****args,
            clos_type_t **arg_types)
{
  closure_t clos = (closure_t) malloc(sizeof(struct closure));

  // Arguments
  *args = malloc(argc*sizeof(void**));
  for (int i = 0; i < argc; i++)
    {
      (*args)[i] = malloc(sizeof(void*));
    }

  // Argument types
  *arg_types = (clos_type_t*)malloc(argc*sizeof(clos_type_t));

  clos->fn = fn;
  clos->res_type = res_type;
  clos->argc = argc;
  clos->args = *args;
  clos->arg_types = *arg_types;
  clos->is_end = false;
  clos->next = NULL;

  return clos;
}

bool
closure_is_end(closure_t clos)
{
  return clos->is_end;
}

void
closure_setup(closure_t clos)
{
  ffi_type *ffi_res_type = (ffi_type *)clos->res_type;
  ffi_type **ffi_arg_types = (ffi_type **)clos->arg_types;

  assert(ffi_prep_cif(&clos->cif,
                      FFI_DEFAULT_ABI,
                      clos->argc,
                      ffi_res_type,
                      ffi_arg_types) == FFI_OK);
}

clos_type_t
closure_void_type ()
{
  return (clos_type_t)&ffi_type_void;
}

clos_type_t
closure_sint_type ()
{
  return (clos_type_t)&ffi_type_sint;
}

clos_type_t
closure_pointer_type ()
{
  return (clos_type_t)&ffi_type_pointer;
}

void
closure_apply(closure_t clos, void* res)
{
  closure_setup(clos);
  ffi_call(&clos->cif, clos->fn, res, (void**)clos->args);
}

void
closure_free(closure_t clos)
{  
  free(clos->arg_types);

  for (int i = 0; i < clos->argc; i++)
    {
      free(clos->args[i]);
    }

  free(clos->args);
  free(clos);
}
