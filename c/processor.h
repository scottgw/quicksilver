#ifndef __PROCESSOR_H_
#define __PROCESSOR_H_

#include <stdio.h>

#include "types.h"

processor_t
make_processor();

void
reset_stack_to(void (*)(void*), processor_t);

void
yield_to_processor(executor_t, processor_t);

void
free_processor(processor_t);

void
proc_sleep(processor_t, struct timespec);

int
proc_running(processor_t);

void
proc_start(processor_t, executor_t);

void
maybe_yield(processor_t, int);

#endif // __PROCESSOR_H_
