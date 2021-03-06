#ifndef _TASK_H
#define _TASK_H

#include "../libqs/types.h"

#ifdef __cplusplus
extern "C" {
#endif

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
  struct task* next;
};


task_t
task_make();

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

#ifdef __cplusplus
}
#endif


#endif // _TASK_H
