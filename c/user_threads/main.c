#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

#define STACKSIZE 4*1024*sizeof(int)

#define SWAPSTACK(val, other_stack)\
  asm volatile (\
    "    call 1f                  # push PC                     \n"\
    "1:  addq $(2f-1b), 0(%%rsp)  # add offset to pushed PC     \n"\
    "    xchgq %%rsp, %%rcx       # swap stacks                 \n"\
    "    ret                      # pop PC on new stack         \n"\
    "2:                                                          \n"\
    : "=a" (val), "=c" (other_stack)                                   \
    : "a" (val), "c" (other_stack)                                     \
    : "%rbx","%rdx","%rbp","%rsi","%rdi","%r8","%r9","%r10","%r11","%r12","%r13","%r14","%r15","memory","cc");


#define SWAPPROC(val, other_proc) SWAPSTACK((val), (other_proc->stack))

static void thread_termination(){
  printf("Thread terminated\n");
}
void* thread_return_addr = (void*)&thread_termination;

typedef void* stack_t;


#define NEWSTACK(val, other_stack) \
  asm volatile (\
    "    call 1f                  # push PC                     \n"\
    "1:  addq $(2f-1b), 0(%%rsp)  # add offset to pushed PC     \n"\
    "    xchgq %%rsp, %%rcx       # swap stacks                 \n"\
    "    movq %%rcx, %%rdi        # set up call                 \n"\
    "    push thread_return_addr  # 'return address'            \n"\
    "    jmp *%%rax               # do the call                 \n"\
    "2:                                                          \n"\
    : "=a" (val), "=c" (other_stack)                                   \
    : "a" (val), "c" (other_stack)                                     \
    : "%rbx","%rdx","%rbp","%rsi","%rdi","%r8","%r9","%r10","%r11","%r12","%r13","%r14","%r15","%rsp","memory","cc");

  

typedef struct 
{
  stack_t base;
  // I don't really know if both of these are needed,
  // but it seems that a copy of the original base should
  // be kept, as 'stack' will jump around.
  stack_t stack;
} processor_t;

typedef struct 
{
  processor_t curr_proc;
} executor_t;

typedef void (*threadfunc)(stack_t);

void
make_processor(processor_t *proc);

void
reset_stack_to(threadfunc f, processor_t *proc);

// The first thing that the argument 'f' should do
// is call SWAPSTACK to switch back to this creation routine.
void
make_processor(processor_t *proc)
{
  proc->base = malloc(STACKSIZE);
}

void
reset_stack_to(threadfunc f, processor_t *proc)
{
  proc->stack = proc->base + STACKSIZE;
  NEWSTACK(f, proc->stack);
}

processor_t *proc1;
processor_t *proc2;

void
thread2(stack_t starter)
{
  int x;
  SWAPSTACK(x, starter);
  printf("Thread2 on the go\n");
  SWAPSTACK(x, starter);
}


void
thread1(stack_t starter)
{
  int x;
  SWAPSTACK(x, starter);
  printf("Before the switch!\n");
  printf("Switching to thread 2!\n");
  SWAPPROC(x, proc2);
  printf("After switch\n");
  SWAPSTACK(x, starter);
}

int
main(int argc, char **argv)
{
  proc1 = malloc(sizeof(processor_t));
  proc2 = malloc(sizeof(processor_t));

  make_processor(proc1);
  make_processor(proc2);
  
  for(int i = 0; i < 10000000; i++)
    {
      int x;
      reset_stack_to(thread1, proc1);
      reset_stack_to(thread2, proc2);

      SWAPPROC(x, proc1);
    }

  free(proc1);
  free(proc2);
  return 0;
}
