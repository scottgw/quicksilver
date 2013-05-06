#ifndef _TYPES_H
#define _TYPES_H

#include <stdbool.h>
#include <stdint.h>
#include <pthread.h>

#define STACKSIZE 4*1024*sizeof(int)

// user stacks
typedef void* user_stack_t;

// lists
struct list;
typedef struct list* list_t;

// bounded queue declarations
struct bounded_queue;
typedef struct bounded_queue* bounded_queue_t;

// private queues
struct priv_queue;
typedef struct priv_queue* priv_queue_t;

// global sync data
struct sync_data;
typedef struct sync_data* sync_data_t;

// processor
struct processor;
typedef struct processor* processor_t;

// executor
struct executor;
typedef struct executor* executor_t;

// task
struct task;
typedef struct task* task_t;

// notifier
struct notifier;
typedef struct notifier* notifier_t;


// closure types
struct closure;
typedef struct closure* closure_t;

struct clos_type;
typedef struct clos_type* clos_type_t;


#endif // _TYPES_H
