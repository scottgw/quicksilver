#ifndef _TASK_H
#define _TASK_H

#include "ctx.h"
#include "types.h"

typedef enum {TASK_UNINIT,
              TASK_RUNNING,
              TASK_RUNNABLE,
              TASK_FINISHED,
              TASK_WAITING}
  task_state;

struct task
{
  user_stack_t base;
  ctx_t ctx;  
  volatile task_state state;
  sync_data_t sync_data;
  struct task* next;
};


task_t
task_make(sync_data_t sync_data);

void
task_free(task_t task);

void
task_set_func(task_t task, void (*f)(void*), void* data);

void
task_run(task_t task);

void
yield_to(task_t from_task, task_t to_task);

#endif // _TASK_H
