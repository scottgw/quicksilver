#ifndef _CLOSURE_H
#define _CLOSURE_H
#include <ffi.h>

#include "types.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum 
{
  CLOS_NORMAL,
  CLOS_END,
  CLOS_WAIT_SYNC,
  CLOS_SYNC
} clos_mode;

struct closure
{
  void *fn;
  clos_type_t res_type;
  int argc;
  void ***args;
  clos_type_t *arg_types;
  
  clos_mode mode;

  closure_t next;
};

closure_t
closure_new_end();

void
closure_new_sync(closure_t clos, processor_t client);

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

bool
closure_is_sync(closure_t clos);

bool
closure_is_wait_sync(closure_t clos);

clos_type_t
closure_uint1_type ();

clos_type_t
closure_sint8_type ();

clos_type_t
closure_sint16_type ();

clos_type_t
closure_sint32_type ();

clos_type_t
closure_sint_type ();

clos_type_t
closure_double_type ();

clos_type_t
closure_void_type ();

clos_type_t
closure_pointer_type ();

void closure_apply(closure_t clos, void* res);


#ifdef __cplusplus
}
#endif


#endif // _CLOSURE_H
