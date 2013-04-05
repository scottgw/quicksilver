#include <stdlib.h>
#include <stdint.h>

#include "processor.h"

static
void
thread_end()
{
  printf("light-weight thread terminated\n");
}

void* thread_return_addr = (void*) thread_end;

// The first thing that the argument 'f' should do
// is call SWAPSTACK to switch back to this creation routine.
processor_t *
make_processor()
{
  processor_t *proc = (processor_t*) malloc(sizeof(processor_t));
  proc->base = malloc(STACKSIZE);
  return proc;
}

void
free_processor(processor_t* proc)
{
  free (proc);
}

void
reset_stack_to(threadfunc f, processor_t *proc)
{
  proc->stack = proc->base + STACKSIZE;
  NEWSTACK(f, proc->stack);
}


int
swap_proc(processor_t* proc)
{
  uint64_t ret = 0;
  SWAPSTACK(ret, proc->stack);
  return ret;
}
