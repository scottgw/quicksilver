#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "task.h"

task_t
task_make(sync_data_t sync_data)
{
  task_t task = (task_t) malloc(sizeof(struct task));
  task->base = malloc(STACKSIZE);
  task->state = TASK_UNINIT;
  task->sync_data = sync_data;

  task->ctx = ctx_new();
  ctx_set_stack_ptr(task->ctx, task->base);
  ctx_set_stack_size(task->ctx, STACKSIZE);
  
  return task;
}

void
task_free(task_t task)
{
  ctx_free(task->ctx);
  free(task->base);
  free(task);
}

typedef struct
{
  task_t task;
  void (*f)(void*);
  void* ptr;
} wrapper_data;

static
void
task_wrapper(wrapper_data* data)
{
  task_t task = data->task;
  void (*f)(void*) = data->f;
  void* ptr = data->ptr;
  free(data);

  task->state = TASK_RUNNING;
  f(ptr);
  task->state = TASK_FINISHED;

  if (task->next != NULL) {
    task_run (task->next);
  }
}

void
task_set_func(task_t task, void (*f)(void*), void* data)
{
  wrapper_data* wrap_data = (wrapper_data*)malloc(sizeof(wrapper_data));
  assert (task != NULL);

  wrap_data->task = task;
  wrap_data->f = f;
  wrap_data->ptr = data;
  
  ctx_make(task->ctx, (void (*)(void*))task_wrapper, wrap_data);
  task->state = TASK_RUNNABLE;
}

void
task_run(task_t task)
{
  task->state = TASK_RUNNING;
  assert(ctx_set(task->ctx) == 0);
}

void
yield_to(task_t from_task, task_t to_task)
{
  assert (from_task->state == TASK_RUNNING || from_task->state == TASK_WAITING);
  volatile bool flag = ctx_save(from_task->ctx);
  if (flag)
    {
      if (from_task->state == TASK_RUNNING)
        from_task->state = TASK_RUNNABLE;
      task_run(to_task);
    }
}
