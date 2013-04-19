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
yield_to_executor(processor_t proc)
{
  yield_to(proc->task, proc->executor->task);
}

void
proc_sleep(processor_t proc, struct timespec duration)
{
  sync_data_add_sleeper(proc->executor->sync_data,
                        proc,
                        duration);
  yield_to(proc->task, proc->executor->task);
}

// The first thing that the argument 'f' should do
// is call SWAPSTACK to switch back to this creation routine.
processor_t
make_processor()
{
  processor_t proc = (processor_t) malloc(sizeof(struct processor));

  proc->task = task_make();
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
