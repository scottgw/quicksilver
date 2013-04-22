#ifndef _TYPES_H
#define _TYPES_H

#include <stdbool.h>
#include <stdint.h>
#include <pthread.h>

#include "ctx.h"
#include "liblfds611.h"

#define STACKSIZE 4*1024*sizeof(int)

// some shorter type names for the LFDS structures
typedef struct lfds611_queue_state* conc_queue_t;

typedef struct lfds611_slist_state* conc_list_t;
typedef struct lfds611_slist_element* conc_list_elem_t;


// global sync data
struct sync_data
{
  lfds611_atom_t max_tasks;

  volatile uint64_t num_processors;

  conc_list_t sleep_list;

  conc_queue_t runnable_queue;
  volatile uint64_t run_queue_size;
  pthread_mutex_t run_mutex;
  pthread_cond_t not_empty;
};

typedef struct sync_data* sync_data_t;


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
              TASK_FINISHED,
              TASK_WAITING}
  task_state;

struct task
{
  user_stack_t base;
  ctx_t ctx;  
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

  sync_data_t sync_data;
  volatile bool done;
  processor_t current_proc;
  pthread_t thread;
};

struct notifier
{
  sync_data_t sync_data;
  pthread_t thread;
};

typedef struct notifier* notifier_t;


#endif // _TYPES_H
