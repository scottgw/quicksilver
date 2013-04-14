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
  getcontext(&task->ctx);
  task->ctx.uc_stack.ss_sp   = task->base + STACKSIZE;
  task->ctx.uc_stack.ss_size = STACKSIZE;

  return task;
}

void
task_free(task_t task)
{
  free(task->base);
  free(task);
}

static
void
task_wrapper(task_t task, void (*f)(void*), void* data)
{
  task->state = TASK_RUNNING;
  f(data);
  task->state = TASK_FINISHED;

  if (task->next != NULL)
    task_run (task->next);
}

void
task_set_func(task_t task, void (*f)(void*), void* data)
{
  assert (task != NULL);
  makecontext(&task->ctx, (void (*)())task_wrapper, 3, task, f, data);
}

void
task_run(task_t task)
{
  task->state = TASK_RUNNING;
  setcontext(&task->ctx);
}

void
yield_to(task_t from_task, task_t to_task)
{
  assert (from_task->state == TASK_RUNNING);
  volatile int flag = 0;
  getcontext(&from_task->ctx);
  if (flag == 0)
    {
      flag = 1;
      from_task->state = TASK_RUNNABLE;
      task_run(to_task);
    }
}
