#ifndef __PROCESSOR_H_
#define __PROCESSOR_H_

#include <stdio.h>

#include "types.h"

typedef void* user_stack_t;

typedef void (*threadfunc)(user_stack_t);

processor_t
make_processor();

void
reset_stack_to(proc_func, processor_t);

void
yield_to_processor(processor_t);

void
free_processor(processor_t);

void
maybe_yield(processor_t);

#endif // __PROCESSOR_H_
