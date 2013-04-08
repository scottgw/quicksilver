#include <stdlib.h>
#include <stdint.h>
#include <ucontext.h>
#include <assert.h>

#include "processor.h"
#include "executor.h"
#include "notifier.h"

int global_id = 0;

static
void
task_wrapper(processor_t proc)
{
  proc->func(proc);
  proc->state = TASK_FINISHED;

  setcontext(&proc->executor->ctx);
  assert (0 && "task_wrapper: should never get here");
}

void
yield_to_processor(executor_t exec, processor_t proc)
{
  proc->executor = exec;
  exec->has_switched = 0;
  getcontext(&exec->ctx);
  if(exec->has_switched == 0)
    {
      exec->has_switched = 1;
      setcontext(&proc->ctx);
    }
}


// Yields to the executor (via its stack).
void
yield_to_executor(processor_t proc)
{
  proc->has_switched = 0;
  proc->state = TASK_RUNNABLE;
  getcontext(&proc->ctx);
  if (proc->has_switched == 0)
    {
      proc->has_switched = 1;
      setcontext(&proc->executor->ctx);
    }
}

void
maybe_yield(processor_t proc, int i)
{
  if (time_is_up == 1)
    {
      time_is_up = 0;
      /* printf("%d: yielding\n", proc->id); */
      yield_to_executor(proc);
    }
}

// The first thing that the argument 'f' should do
// is call SWAPSTACK to switch back to this creation routine.
processor_t
make_processor()
{
  processor_t proc = (processor_t) malloc(sizeof(struct processor));

  proc->base = malloc(STACKSIZE);
  proc->func = NULL;
  proc->id = global_id++;
  proc->state = TASK_RUNNABLE;

  return proc;
}

void
free_processor(processor_t proc)
{
  free (proc->base);
  free (proc);
}

void
reset_stack_to(void (*f)(processor_t), processor_t proc)
{
  proc->func = f;

  getcontext(&proc->ctx);

  proc->ctx.uc_stack.ss_sp   = proc->base + STACKSIZE;
  proc->ctx.uc_stack.ss_size = STACKSIZE;

  makecontext(&proc->ctx, (void (*)())task_wrapper, 1, proc);
}
