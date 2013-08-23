#ifndef _TYPES_H
#define _TYPES_H

#include <stdbool.h>
#include <stdint.h>

#define STACKSIZE 4*1024*sizeof(int)

// user stacks
typedef void* user_stack_t;

//
struct ctx;
typedef struct ctx* ctx_t;

// lists
struct list;
typedef struct list* list_t;

// bounded queue declarations
struct bounded_queue;
typedef struct bounded_queue* bounded_queue_t;

// single supplier/consumer queue declarations
struct spsc_queue;
typedef struct spsc_queue* spsc_queue_t;


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

// sched task
struct sched_task;
typedef struct sched_task* sched_task_t;

// notifier
struct notifier;
typedef struct notifier* notifier_t;

// task mutex
struct task_mutex;
typedef struct task_mutex* task_mutex_t;

// task condition
struct task_condition;
typedef struct task_condition* task_condition_t;


// closure types
struct closure;
typedef struct closure* closure_t;

struct clos_type;
typedef struct clos_type* clos_type_t;

// general queue
struct queue_impl;
typedef struct queue_impl* queue_impl_t;

// work stealing
struct ws_deque;
typedef struct ws_deque* ws_deque_t;

// MPSC queues
struct mpscq_node;
typedef struct mpscq_node mpscq_node_t;

struct mpscq;
typedef struct mpscq mpscq_t;

// closure queues (MPSC)
struct qoq;
typedef struct qoq* qoq_t;


#endif // _TYPES_H
