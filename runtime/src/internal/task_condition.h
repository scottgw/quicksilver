#ifndef _TASK_CONDITION_H
#define _TASK_CONDITION_H

#include "../libqs/types.h"

#ifdef __cplusplus
extern "C" {
#endif

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

#ifdef __cplusplus
}
#endif

#endif // _TASK_CONDITION_H
