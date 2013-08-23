#ifndef _TASK_CONDITION_H
#define _TASK_CONDITION_H

#include "types.h"

struct task_condition;
typedef struct task_condition* task_condition_t;

task_condition_t
task_condition_new();

void
task_condition_free(task_condition_t cv);

void
task_condition_signal(task_condition_t cv, sched_task_t curr_stask);

void
task_condition_signal_all(task_condition_t cv, sched_task_t curr_stask);

void
task_condition_wait(task_condition_t cv, task_mutex_t mutex, sched_task_t prop);

#endif // _TASK_CONDITION_H
