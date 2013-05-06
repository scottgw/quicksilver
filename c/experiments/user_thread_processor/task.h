#ifndef _TASK_H
#define _TASK_H

#include "types.h"

task_t
task_make();

void
task_free(task_t task);

void
task_set_func(task_t task, void (*f)(void*), void* data);

void
task_run(task_t task);

void
yield_to(task_t from_task, task_t to_task);

#endif // _TASK_H
