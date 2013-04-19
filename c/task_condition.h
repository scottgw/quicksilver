#ifndef _TASK_CONDITION_H
#define _TASK_CONDITION_H

#include "types.h"

struct task_condition;
typedef struct task_condition* task_condition_t;

task_condition_t
task_condition_new(processor_t proc);

#endif // _TASK_CONDITION_H
