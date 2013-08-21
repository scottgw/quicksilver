#ifndef _TASK_H
#define _TASK_H

#include "types.h"

typedef enum 
  {
    TASK_UNINIT,
    TASK_RUNNING,
    TASK_RUNNABLE,
    TASK_FINISHED,
    TASK_WAITING,
    TASK_TRANSITION_TO_WAITING,
    TASK_TRANSITION_TO_RUNNABLE,
    TASK_TRANSITION_TO_FINISHED
  }
  task_state;

struct task
{
  ctx_t ctx;  
  volatile task_state state;
  sync_data_t sync_data;
  struct task* next;
};


task_t
task_make(sync_data_t sync_data);

void
task_free(task_t task);

task_state
task_get_state(task_t task);

void
task_set_state(task_t task, task_state state);

void
task_set_func(task_t task, void (*f)(void*), void* data);

void
task_set_func_and_run(task_t task, void (*f)(void*), void* data);

void
task_switch(task_t from_task, task_t to_task);

#endif // _TASK_H
