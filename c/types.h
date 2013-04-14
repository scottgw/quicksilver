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

typedef enum {TASK_UNINIT,
              TASK_RUNNING,
              TASK_RUNNABLE,
              TASK_FINISHED}
  task_state;

struct task
{
  user_stack_t base;
  ucontext_t ctx;  
  volatile task_state state;
  struct task* next;
};

typedef struct task* task_t;

struct processor
{
  task_t task;
  executor_t executor;
  int id;  
};

struct executor
{
  task_t task;

  list_t work;
  processor_t current_proc;
  pthread_t thread;
};



#endif // _TYPES_H
