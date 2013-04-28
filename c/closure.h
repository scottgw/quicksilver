#ifndef _CLOSURE_H
#define _CLOSURE_H
#include <ffi.h>

struct closure;
typedef struct closure* closure_t;

closure_t
closure_new(ffi_cif *cif, void *fn, int argc, void **args);

void closure_apply(closure_t clos, void* res);

#endif // _CLOSURE_H
