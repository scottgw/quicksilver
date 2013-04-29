#ifndef _CLOSURE_H
#define _CLOSURE_H
#include <ffi.h>

struct closure;
typedef struct closure* closure_t;

struct clos_type;
typedef struct clos_type* clos_type_t;

closure_t
closure_new_end();

closure_t
closure_new(void *fn,
            clos_type_t res_type,
            int argc,
            void ****args,
            clos_type_t **arg_types);

bool
closure_is_end(closure_t clos);

void
closure_setup(closure_t clos);

clos_type_t
closure_void_type ();

clos_type_t
closure_sint_type ();

clos_type_t
closure_pointer_type ();

void closure_apply(closure_t clos, void* res);

#endif // _CLOSURE_H
