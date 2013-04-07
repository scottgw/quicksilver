#include <stdlib.h>
#include <stdint.h>
#include <setjmp.h>
#include <assert.h>

#include "processor.h"
#include "executor.h"
#include "notifier.h"

#define STACKSIZE 4*1024*sizeof(int)

#define SWAPSTACK(val, other_stack)                                     \
  asm volatile (                                                        \
                "    call 1f                 # push IP           \n"    \
                "1:  addq $(2f-1b), 0(%%rsp) # jump past xchg PC \n"    \
                "    xchgq %%rsp, %%rcx      # swap stacks       \n"    \
                "    ret                     # pop to new stack  \n"    \
                "2:\n"                                                  \
                : "=a"(val), "=c" (other_stack)                         \
                : "a"(val), "c" (other_stack)                           \
                : "%rbx","%rdx","%rbp","%rsi","%rdi","%r8",             \
                  "%r9","%r10", "%r11","%r12","%r13","%r14","%r15",     \
                  "memory","cc");


extern void* thread_return_addr;

#define NEWSTACK(func, other_stack)                                     \
  asm volatile (                                                        \
                "    call 1f                  # push PC\n"              \
                "1:  addq $(2f-1b), 0(%%rsp)  # jump past xchg\n"       \
                "    xchgq %%rsp, %%rcx       # swap stacks \n"         \
                "    movq %%rcx, %%rdi        # set up call \n"         \
                "    push thread_return_addr  # 'return address' \n"    \
                "    jmp *%%rax               # do the call \n"         \
                "2:\n"                                                  \
                : "=a" (func), "=c" (other_stack)                       \
                : "a" (func), "c" (other_stack)                         \
                : "%rbx","%rdx","%rbp","%rsi","%rdi","%r8","%r9",       \
                  "%r10","%r11","%r12","%r13","%r14","%r15","%rsp",     \
                  "memory","cc");


int global_id = 0;

struct processor
{
  // I don't really know if both of these are needed,
  // but it seems that a copy of the original base should
  // be kept, as 'stack' will jump around.
  user_stack_t stack;

  user_stack_t executor_stack;

  // The stack base, so we can reset it.
  user_stack_t base;

  int id;
  
  int is_done;
  void (*func)(processor_t);
};


static
void
thread_end()
{
  printf("light-weight thread terminated\n");
}

static
void
task_wrapper(user_stack_t starter)
{
  processor_t proc = NULL;

  // This initial swap goes back to the starting thread.
  SWAPSTACK(proc, starter);
  
  // When we're rescheduled, the proc will be 'us', as it will
  // be kicked off by an executor.
  printf("starting func %d\n", proc->id);

  proc->executor_stack = starter;
  proc->func(proc);

  printf ("func is done %d\n", proc->id);

  proc->is_done = 1;

  SWAPSTACK(proc, starter);

  assert (0 && "task_wrapper: should never get here");
}

// Yields to the executor (via its stack).
void
yield_to_executor(processor_t proc)
{
  user_stack_t stack = proc->executor_stack;

  // We could send proc or some other information,
  // proc is redundant as the executor already
  // has proc as its current_proc.
  SWAPSTACK(proc, stack);

  proc->executor_stack = stack;
}

void
maybe_yield(processor_t proc)
{
  if (time_is_up)
    {
      time_is_up = 0;
      /* printf("Yielding\n"); */
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
  proc->executor_stack = NULL;
  global_id++;
  proc->id = global_id;
  return proc;
}

void
free_processor(processor_t proc)
{
  free (proc);
}

void
reset_stack_to(void (*f)(), processor_t proc)
{
  void (*wrapper_lval)() = task_wrapper;
  proc->stack = proc->base + STACKSIZE;  
  proc->func = f;
  NEWSTACK(wrapper_lval, proc->stack);
}

void
yield_to_processor(processor_t proc)
{
  user_stack_t stack = proc->stack;
  printf ("swapping %d\n", proc->id);
  SWAPSTACK(proc, stack);
  assert (proc != NULL);
  printf ("swapped %d\n", proc->id);
}


// setjmp constants that were taken out of GLIBC at some point
// this is guesswork mostly, so it will be vulerable if it changes.

/* #if __GLIBC__ == 2 || defined(__UCLIBC__)      /\* arm-linux-glibc2 *\/  */
/* #ifndef __JMP_BUF_SP  */
/* #define __JMP_BUF_SP   ((sizeof(__jmp_buf)/sizeof(int))-2)  */
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
