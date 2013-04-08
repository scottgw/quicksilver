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
thread_end()
{
  printf("light-weight thread terminated\n");
}

static
void
task_wrapper(processor_t proc)
{
  printf("starting func %d\n", proc->id);
  proc->func(proc);
  printf ("func is done %d\n", proc->id);
  proc->is_done = 1;

  setcontext(&proc->executor->ctx);
  assert (0 && "task_wrapper: should never get here");
}

void
yield_to_processor(executor_t exec, processor_t proc)
{
  /* printf ("swapping %d\n", proc->id); */

  proc->executor = exec;
  exec->has_switched = 0;
  getcontext(&proc->executor->ctx);
  if(exec->has_switched == 0)
    {
      exec->has_switched++;
      setcontext(&proc->ctx);
    }
  exec->has_switched++;
  /* printf("coming back from yielding to proc %d\n", exec->has_switched); */
  /* assert (proc != NULL); */
  /* printf ("swapped %d\n", proc->id); */
}


// Yields to the executor (via its stack).
void
yield_to_executor(processor_t proc)
{
  proc->has_switched = 0;
  getcontext(&proc->ctx);
  if (proc->has_switched == 0)
    {
      proc->has_switched++;
      setcontext(&proc->executor->ctx);
    }
  proc->has_switched++;
  /* printf("coming back from yielding to exec %d\n", proc->has_switched); */
}

int
proc_running(processor_t proc)
{
  return proc->is_running;
}

void
proc_start(processor_t proc, executor_t exec)
{
  printf("starting proc\n");
  proc->is_running = 1;
  proc->executor = exec;
  exec->has_switched = 0;

  getcontext(&proc->executor->ctx);
  if(exec->has_switched == 0)
    {
      exec->has_switched++;
      setcontext(&proc->ctx);
    }
}

void
maybe_yield(processor_t proc, int i)
{
  if (time_is_up == 1)
    {
      time_is_up = 0;
      printf("Yielding %d\n", i);
      yield_to_executor(proc);
    }
}

void* thread_return_addr = (void*) thread_end;

// The first thing that the argument 'f' should do
// is call SWAPSTACK to switch back to this creation routine.
processor_t
make_processor()
{
  processor_t proc = (processor_t) malloc(sizeof(struct processor));

  proc->base = malloc(STACKSIZE);
  proc->func = NULL;
  proc->id = global_id++;
  proc->is_running = 0;

  return proc;
}

void
free_processor(processor_t proc)
{
  free (proc);
}

// from coroutines from Lua
#define __JMP_BUF_SP   ((sizeof(__jmp_buf)/sizeof(int))-2)
#define __JMP_BUF_IP   (__JMP_BUF_SP + 1)

void
reset_stack_to(void (*f)(processor_t), processor_t proc)
{
  proc->func = f;

  getcontext(&proc->ctx);

  proc->ctx.uc_stack.ss_sp   = proc->base + STACKSIZE;
  proc->ctx.uc_stack.ss_size = STACKSIZE;

  makecontext(&proc->ctx, (void (*)())task_wrapper, 1, proc);
}


// setjmp constants that were taken out of GLIBC at some point
// this is guesswork mostly, so it will be vulerable if it changes.

/* #if __GLIBC__ == 2 || defined(__UCLIBC__)      /\* arm-linux-glibc2 *\/  */
/* #ifndef __JMP_BUF_SP  */

/* #endif  */
/* #define COCO_PATCHCTX(coco, buf, func, stack, a0) \  */
/*   buf->__jmpbuf[__JMP_BUF_SP+1] = (int)(func); /\* pc *\/ \  */
/*   buf->__jmpbuf[__JMP_BUF_SP] = (int)(stack); /\* sp *\/ \  */
/*   buf->__jmpbuf[__JMP_BUF_SP-1] = 0; /\* fp *\/ \  */
/*   stack[0] = (size_t)(a0);  */
/* #define COCO_STACKADJUST       2  */
/* #define COCO_MAIN_PARAM                int _a, int _b, int _c, int _d, lua_State *L  */
/* #elif defined(__APPLE__)       /\* arm-ios *\/  */
/* #define __JMP_BUF_SP  7   /\* r4 r5 r6 r7 r8 r10 fp sp lr sig ... *\/  */
/* #define COCO_PATCHCTX(coco, buf, func, stack, a0) \  */
/*   buf[__JMP_BUF_SP+1] = (int)(func); /\* lr *\/ \  */
/*   buf[__JMP_BUF_SP] = (int)(stack); /\* sp *\/ \  */
/*   buf[__JMP_BUF_SP-1] = 0; /\* fp *\/ \  */
/*   stack[0] = (size_t)(a0);  */
/* #define COCO_STACKADJUST 2  */
/* #define COCO_MAIN_PARAM int _a, int _b, int _c, int _d, lua_State *L  */
/* #endif  */
 
/* #endif /\* arch check *\/  */
 
/* #ifdef COCO_PATCHCTX  */
/* #define COCO_CTX               jmp_buf  */
/* #define COCO_MAKECTX(coco, buf, func, stack, a0) \  */
/*   _setjmp(buf); COCO_PATCHCTX(coco, buf, func, stack, a0)  */
/* #define COCO_SWITCH(from, to)  if (!_setjmp(from)) _longjmp(to, 1);  */
