#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "internal/task.h"
#include "internal/ctx.h"

task_t
task_make()
{
  task_t task = (task_t) malloc(sizeof(struct task));

  task->next = NULL;
  task->state = TASK_UNINIT;
  task->ctx = ctx_new();

  return task;
}

void
task_free(task_t task)
{
  ctx_free(task->ctx);
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
task_run(task_t task)
{
  /* printf("task_run\n"); */
  task->state = TASK_RUNNING;
  int result = ctx_set(task->ctx);
  assert(result == 0);
}


static
void
task_wrapper(wrapper_data* data)
{
  task_t task = data->task;
  void (*f)(void*) = data->f;
  void* ptr = data->ptr;
  free(data);

  /* printf("task_wrapper\n"); */
  task->state = TASK_RUNNING;
  f(ptr);
  task->state = TASK_TRANSITION_TO_FINISHED;

  if (task->next != NULL) {
    task_run (task->next);
  }
}

task_state
task_get_state(task_t task)
{
  task_state state;
  __atomic_load(&task->state, &state,__ATOMIC_SEQ_CST);
  return state;
}


void
task_set_state(task_t task, task_state state)
{
  task->state = state;
  /* return __atomic_store_4(&task->state, state, __ATOMIC_SEQ_CST); */
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
task_set_func_and_run(task_t task, void (*f)(void*), void* data)
{
  task_set_func(task, f, data);
  task_run(task);
}

void
task_switch(task_t from_task, task_t to_task)
{
  /* printf("task_yield\n"); */
  /* assert(from_task->state >= TASK_TRANSITION_TO_WAITING); */
  volatile bool flag = ctx_save(from_task->ctx);
  if (flag)
    {
      task_run(to_task);
    }
}
