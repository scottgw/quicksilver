#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>

#include "libqs/closure.h"
#include "libqs/private_queue.h"

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
  size_t total_size = 
    sizeof(struct closure) + 
    argc*(sizeof(void**) +
          sizeof(void*) +
          sizeof(clos_type_t));
  void* mem = malloc(total_size);
  closure_t clos = mem;

  mem += sizeof(*clos);

  // Arguments
  *args = mem;
  mem += argc * sizeof(void**);
  for (int i = 0; i < argc; i++)
    {
      (*args)[i] = mem;
      mem += sizeof(void*);
    }

  // Argument types
  *arg_types = mem;

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

clos_type_t
closure_void_type ()
{
  return (clos_type_t)&ffi_type_void;
}

clos_type_t
closure_sint_type ()
{
  return (clos_type_t)&ffi_type_sint64;
}

clos_type_t
closure_pointer_type ()
{
  return (clos_type_t)&ffi_type_pointer;
}

void
closure_apply(closure_t clos, void* res)
{
  if (clos->fn == function_wrapper)
    {
      void (*fn)(closure_t, void*, processor_t);
      fn = clos->fn;
      fn(*((closure_t*)clos->args[0]),
         *(clos->args[1]),
         *((processor_t*)clos->args[2]));
    }
  else
    {
      ffi_type *ffi_res_type = (ffi_type *)clos->res_type;
      ffi_type **ffi_arg_types = (ffi_type **)clos->arg_types;
      ffi_cif cif;

      int result = ffi_prep_cif(&cif,
                                FFI_DEFAULT_ABI,
                                clos->argc,
                                ffi_res_type,
                                ffi_arg_types);
        
      assert(result == FFI_OK);

      ffi_call(&cif, clos->fn, res, (void**)clos->args);
    }
}

void
closure_free(closure_t clos)
{
  free(clos);
}
