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
  clos->mode = CLOS_END;
  return clos;
}


void
closure_new_sync(closure_t clos, processor_t client)
{
  clos->mode = CLOS_SYNC;
  clos->fn = (void*) client;
  clos->next = NULL;
}

void
closure_new_wait_sync(closure_t clos, processor_t client)
{
  clos->mode = CLOS_WAIT_SYNC;
  clos->fn = (void*) client;
  clos->next = NULL;
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
  clos->mode = CLOS_NORMAL;
  clos->next = NULL;

  return clos;
}

bool
closure_is_end(closure_t clos)
{
  return clos->mode == CLOS_END;
}

bool
closure_is_sync(closure_t clos)
{
  return clos->mode == CLOS_SYNC || clos->mode == CLOS_WAIT_SYNC;
}

bool
closure_is_wait_sync(closure_t clos)
{
  return clos->mode == CLOS_WAIT_SYNC;
}

clos_type_t
closure_void_type ()
{
  return (clos_type_t)&ffi_type_void;
}


clos_type_t
closure_uint1_type ()
{
  return (clos_type_t)&ffi_type_uint8;
}


clos_type_t
closure_sint8_type ()
{
  return (clos_type_t)&ffi_type_sint8;
}

clos_type_t
closure_sint16_type ()
{
  return (clos_type_t)&ffi_type_sint16;
}

clos_type_t
closure_sint32_type ()
{
  return (clos_type_t)&ffi_type_sint32;
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

void
closure_free(closure_t clos)
{
  free(clos);
}
