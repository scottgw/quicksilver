#ifndef _TASK_MUTEX_H
#define _TASK_MUTEX_H

#include "../libqs/types.h"

#ifdef __cplusplus
extern "C" {
#endif

task_mutex_t
task_mutex_new();

void
task_mutex_free();

sched_task_t
task_mutex_owner(task_mutex_t mutex);

void
task_mutex_lock(task_mutex_t mutex, sched_task_t stask);

void
task_mutex_unlock(task_mutex_t mutex, sched_task_t stask);

#ifdef __cplusplus
}
#endif

#endif // _TASK_MUTEX_H
