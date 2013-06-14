#include <assert.h>
#include <stdlib.h>

#include "libqs/debug_log.h"
#include "libqs/processor.h"
#include "libqs/mpsc_impl.h"
#include "libqs/sync_ops.h"
#include "libqs/task.h"
#include "libqs/task_mutex.h"
#include "libqs/types.h"

#define INIT_WAIT_QUEUE_SIZE 16000

struct task_mutex 
{
  volatile uint32_t count;
  volatile processor_t owner;
  mpscq_node_t stub;
  mpscq_t wait_queue;
};

task_mutex_t
task_mutex_new()
{
  task_mutex_t mutex = (task_mutex_t)malloc(sizeof(struct task_mutex));
  mutex->count = 0;
  mutex->owner = NULL;
  mpscq_create(&mutex->wait_queue, &mutex->stub);

  return mutex;
}

void
task_mutex_free(task_mutex_t mutex)
{
  free (mutex);
}

processor_t
task_mutex_owner(task_mutex_t mutex)
{
  return mutex->owner;
}

/* void */
/* task_mutex_lock(task_mutex_t mutex, processor_t proc) */
/* { */
/*  if (__sync_fetch_and_add(&mutex->count, 1) > 0) */
/*     { */
/*       // if the owner is already set then we add to the wait list */
/*       // and yield to the executor. */
/*       proc->task->state = TASK_TRANSITION_TO_WAITING; */
/*       assert(queue_impl_enqueue(mutex->wait_queue, proc)); */
/*       yield_to_executor(proc); */
/*     } */
/*  else */
/*    { */
/*      // if not, set the owner. */
/*      __sync_synchronize(); */
/*      mutex->owner = proc; */
/*    } */
/* } */

/* void */
/* task_mutex_unlock(task_mutex_t mutex, processor_t proc) */
/* { */
/*   processor_t other_proc = NULL; */
/*   assert(mutex->owner == proc); */
  
/*   if (__sync_fetch_and_sub(&mutex->count, 1) > 1) */
/*     { */
/*       // if there's someone in the loop, spin to dequeue them from the wait-queue. */
/*       // spinning is OK here because we only have to wait between when they */
/*       // increased the counter and when the enqueue themselves which */
/*       // is a finite amount of time. */
/*       while(!queue_impl_dequeue(mutex->wait_queue, (void**)&other_proc)); */
/*       __sync_synchronize(); */
/*       while(other_proc->task->state != TASK_WAITING); */
/*       mutex->owner = other_proc; */
/*       task_set_state(other_proc->task, TASK_RUNNABLE); */
/*       sync_data_enqueue_runnable(proc->task->sync_data, other_proc); */
/*     } */
/*   else */
/*     { */
/*       // If there were zero people here, then just set the owner to NULL */
/*       __sync_synchronize(); */
/*       mutex->owner = NULL; */
/*     } */
/* } */

void
task_mutex_lock(volatile task_mutex_t mutex, processor_t proc)
{
  DEBUG_LOG(2, "%p requests mutex %p\n", proc, mutex);

  if (__atomic_add_fetch(&mutex->count, 1, __ATOMIC_RELAXED) > 1)
    {
      // if the owner is already set then we add to the wait list
      // and yield to the executor.
      DEBUG_LOG(2, "%p fails to attain %p\n", proc, mutex);
      /* mpscq_node_t* node = malloc (sizeof(*node)); */
      /* node->state = proc; */
      proc->node.state = proc;
      mpscq_push(&mutex->wait_queue, &proc->node);

      task_set_state(proc->task, TASK_TRANSITION_TO_WAITING);
      proc_yield_to_executor(proc);
      DEBUG_LOG(2, "%p retries mutex %p\n", proc, mutex);
    }

  mutex->owner = proc;
  DEBUG_LOG(2, "%p attains mutex %p\n", proc, mutex);
}

void
task_mutex_unlock(volatile task_mutex_t mutex, processor_t proc)
{
  assert(mutex->owner == proc);
  assert(mutex->count > 0);

  DEBUG_LOG(2, "%p unlocks mutex %p\n", proc, mutex);

  if (__atomic_sub_fetch(&mutex->count, 1, __ATOMIC_RELAXED) > 0)
    {
      DEBUG_LOG(2, "%p found another waiting on mutex %p\n", proc, mutex);
      
      mpscq_node_t* node = mpscq_pop(&mutex->wait_queue);

      while(node == 0)
        {
          node = mpscq_pop(&mutex->wait_queue);
        }
      processor_t other_proc = (processor_t) node->state;
      /* free(node); */
      DEBUG_LOG(2, "%p is awoken out of mutex %p\n", other_proc, mutex);
      // If there's someone in the loop, spin to dequeue them from the
      // wait-queue.
      //
      // Spinning is OK here because we only have to wait between when they
      // increased the counter and when the enqueue themselves which
      // is a finite amount of time.
      proc_wake(other_proc, proc->executor);
    }
}
