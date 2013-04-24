#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "task.h"

task_t
task_make()
{
  task_t task = (task_t) malloc(sizeof(struct task));
  task->base = malloc(STACKSIZE);
  task->state = TASK_UNINIT;
  task->ctx = ctx_new();
  ctx_set_stack_ptr(task->ctx, task->base + STACKSIZE);
  ctx_set_stack_size(task->ctx, STACKSIZE);
  
  return task;
}

void
task_free(task_t task)
{
  free(task->base);
  free(task);
}

typedef struct
{
  task_t task;
  void (*f)(void*);
  void* data;
} wrapper_data;

static
void
task_wrapper(wrapper_data* data)
{
  data->task->state = TASK_RUNNING;
  data->f(data->data);
  data->task->state = TASK_FINISHED;

  if (data->task->next != NULL) {
    printf("running next task\n");
    task_t next = data->task->next;
    free(data);
    task_run (next);
  }
}

void
task_set_func(task_t task, void (*f)(void*), void* data)
{
  wrapper_data* wrap_data = (wrapper_data*)malloc(sizeof(wrapper_data));
  assert (task != NULL);

  wrap_data->task = task;
  wrap_data->f = f;
  wrap_data->data = data;
  
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
