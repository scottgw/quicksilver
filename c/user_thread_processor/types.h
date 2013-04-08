#ifndef _TYPES_H
#define _TYPES_H

#include <ucontext.h>

#define STACKSIZE 4*1024*sizeof(int)

struct list
{
  void** data;
  int start;
  int end;
  int size;
};

typedef struct list* list_t;
typedef void* user_stack_t;

struct processor;
typedef struct processor* processor_t;

struct executor;
typedef struct executor* executor_t;

typedef enum {TASK_RUNNING, TASK_RUNNABLE, TASK_FINISHED} task_state;

struct processor
{
  user_stack_t base;
  executor_t executor;
  ucontext_t ctx;

  volatile task_state state;

  volatile int has_switched;
  int id;  
  void (*func)(processor_t);
};

typedef void (*proc_func)(processor_t);


struct executor
{
  list_t work;
  processor_t current_proc;

  volatile int has_switched;
  ucontext_t ctx;
  pthread_t thread;
};



#endif // _TYPES_H
