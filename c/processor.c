#include <stdlib.h>
#include <stdint.h>
#include <ucontext.h>
#include <assert.h>

#include "processor.h"
#include "executor.h"
#include "notifier.h"
#include "task.h"

int global_id = 0;

void
maybe_yield(processor_t proc, int i)
{
  if (time_is_up == 1)
    {
      time_is_up = 0;
      yield_to(proc->task, proc->executor->task);
    }
}

void
proc_loop(processor_t proc)
{
  
}

void
proc_wake(processor_t proc)
{
  assert(proc->task->state == TASK_WAITING);
  proc->task->state = TASK_RUNNABLE;
  sync_data_enqueue_runnable(proc->task->sync_data, proc);
}

void
yield_to_executor(processor_t proc)
{
  yield_to(proc->task, proc->executor->task);
}

void
proc_sleep(processor_t proc, struct timespec duration)
{
  sync_data_add_sleeper(proc->task->sync_data, proc, duration);
  yield_to(proc->task, proc->executor->task);
}

processor_t
make_processor(sync_data_t sync_data)
{
  processor_t proc = (processor_t) malloc(sizeof(struct processor));

  proc->task = task_make(sync_data);
  proc->id = global_id++;

  return proc;
}

void
free_processor(processor_t proc)
{
  task_free (proc->task);
  free (proc);
}

void
reset_stack_to(void (*f)(void*), processor_t proc)
{
  task_set_func(proc->task, f, proc);
}
