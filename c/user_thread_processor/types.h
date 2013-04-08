#ifndef _TYPES_H
#define _TYPES_H

#include <glib.h>
#include <ucontext.h>

#define STACKSIZE 4*1024*sizeof(int)


struct work_list 
{
  void** data;
  int start;
  int end;
  int size;
};

typedef struct work_list* work_list_t;
typedef void* user_stack_t;

struct processor;
typedef struct processor* processor_t;

struct executor;
typedef struct executor* executor_t;

struct processor
{
  user_stack_t base;
  executor_t executor;
  ucontext_t ctx;

  int is_running;
  int has_switched;
  int id;  
  int is_done;
  void (*func)(processor_t);
};

typedef void (*proc_func)(processor_t);


struct executor
{
  work_list_t work;
  processor_t current_proc;

  int has_switched;
  ucontext_t ctx;
  pthread_t thread;
};



#endif // _TYPES_H
