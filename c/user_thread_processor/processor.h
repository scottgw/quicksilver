#ifndef __PROCESSOR_H_
#define __PROCESSOR_H_

#include <stdio.h>

typedef void* user_stack_t;

#define STACKSIZE 4*1024*sizeof(int)


#define SWAPSTACK(val, other_stack)                                     \
  asm volatile (                                                        \
                "    push %%rbx\n"                                      \
                "    push %%rdx\n"                                      \
                "    push %%r8\n"                                       \
                "    push %%r9\n"                                       \
                "    push %%r10\n"                                      \
                "    push %%r11\n"                                      \
                "    push %%r12\n"                                      \
                "    push %%r13\n"                                      \
                "    push %%r14\n"                                      \
                "    push %%r15\n"                                      \
                "    push %%rbp\n"                                      \
                "    push %%rsi\n"                                      \
                "    push %%rdi\n"                                      \
                "    pushfq\n"                                          \
                "    call 1f                 # push IP           \n"    \
                "1:  addq $(2f-1b), 0(%%rsp) # jump past xchg PC \n"    \
                "    xchgq %%rsp, %%rcx      # swap stacks       \n"    \
                "    ret                     # pop to new stack  \n"    \
                "2:  popfq\n"                                           \
                "    popq %%rdi\n"                                      \
                "    popq %%rsi\n"                                      \
                "    popq %%rbp\n"                                      \
                "    popq %%r15\n"                                      \
                "    popq %%r14\n"                                      \
                "    popq %%r13\n"                                      \
                "    popq %%r12\n"                                      \
                "    popq %%r11\n"                                      \
                "    popq %%r10\n"                                      \
                "    popq %%r9\n"                                       \
                "    popq %%r8\n"                                       \
                "    popq %%rdx\n"                                      \
                "    popq %%rbx\n"                                      \
                : "=a"(val), "=c" (other_stack)                         \
                : "a"(val), "c" (other_stack)                           \
                : "%rbx","%rdx","%rbp","%rsi","%rdi","%r8",             \
                  "%r9","%r10", "%r11","%r12","%r13","%r14","%r15",     \
                  "memory","cc");


#define SWAPPROC(other_proc) SWAPSTACK((other_proc->stack))

#define ENDSTACK(other_stack) SWAPSTACK(other_stack)


extern void* thread_return_addr;

#define NEWSTACK(func, other_stack) \
  asm volatile (\
    "    call 1f                  # push PC                     \n"\
    "1:  addq $(2f-1b), 0(%%rsp)  # add offset to pushed PC     \n"\
    "    xchgq %%rsp, %%rcx       # swap stacks                 \n"\
    "    movq %%rcx, %%rdi        # set up call                 \n"\
    "    push thread_return_addr  # 'return address'            \n"\
    "    jmp *%%rax               # do the call                 \n"\
    "2:                                                          \n"\
    : "=a" (func), "=c" (other_stack)                                   \
    : "a" (func), "c" (other_stack)                                     \
    : "%rbx","%rdx","%rbp","%rsi","%rdi","%r8","%r9","%r10","%r11","%r12","%r13","%r14","%r15","%rsp","memory","cc");

typedef struct 
{
  user_stack_t base;
  // I don't really know if both of these are needed,
  // but it seems that a copy of the original base should
  // be kept, as 'stack' will jump around.
  user_stack_t stack;
} processor_t;

typedef void (*threadfunc)(user_stack_t);

processor_t *
make_processor();

void
reset_stack_to(threadfunc f, processor_t *proc);

void
free_processor(processor_t*);

#endif // __PROCESSOR_H_
