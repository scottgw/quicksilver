#ifndef _CLOSURE_H
#define _CLOSURE_H
#include <ffi.h>

#include "types.h"

struct closure
{
  void *fn;
  clos_type_t res_type;
  int argc;
  void ***args;
  clos_type_t *arg_types;
  
  bool is_end;

  void* next;
};

closure_t
closure_new_end();

closure_t
closure_new(void *fn,
            clos_type_t res_type,
            int argc,
            void ****args,
            clos_type_t **arg_types);

void
closure_free(closure_t clos);

bool
closure_is_end(closure_t clos);

clos_type_t
closure_uint1_type ();

clos_type_t
closure_sint8_type ();

clos_type_t
closure_sint16_type ();

clos_type_t
closure_sint32_type ();

clos_type_t
closure_void_type ();

clos_type_t
closure_sint_type ();

clos_type_t
closure_pointer_type ();

void closure_apply(closure_t clos, void* res);

#endif // _CLOSURE_H
