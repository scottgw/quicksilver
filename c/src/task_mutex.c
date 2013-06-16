#include <assert.h>
#include <stdlib.h>

#include "libqs/debug_log.h"
#include "libqs/processor.h"
/* #include "libqs/mpsc_impl.h" */
#include "libqs/queue_impl.h"
#include "libqs/sync_ops.h"
#include "libqs/task.h"
#include "libqs/task_mutex.h"
#include "libqs/types.h"

#define INIT_WAIT_QUEUE_SIZE 16000

struct task_mutex 
{
  volatile uint32_t count;
  processor_t owner;
  /* struct mpscq_node_t stub; */
  /* mpscq_t wait_queue; */
  queue_impl_t wait_queue;
};

task_mutex_t
task_mutex_new()
{
  task_mutex_t mutex = (task_mutex_t)malloc(sizeof(struct task_mutex));
  mutex->count = 0;
  mutex->owner = NULL;
  /* mpscq_create(&mutex->wait_queue, NULL); // mutex->stub); */
  mutex->wait_queue = queue_impl_new (INIT_WAIT_QUEUE_SIZE);
  return mutex;
}

void
task_mutex_free(task_mutex_t mutex)
{
  queue_impl_free (mutex->wait_queue);
  free (mutex);
}

processor_t
task_mutex_owner(task_mutex_t mutex)
{
  return mutex->owner;
}

void
task_mutex_lock(task_mutex_t mutex, processor_t proc)
{
  DEBUG_LOG(2, "%p requests mutex %p\n", proc, mutex);
  assert (mutex->count < 100);

  if (__atomic_add_fetch(&mutex->count, 1, __ATOMIC_SEQ_CST) > 1)
    {
      // if the owner is already set then we add to the wait list
      // and yield to the executor.
      DEBUG_LOG(2, "%p fails to attain %p\n", proc, mutex);

      /* mpscq_node_t* node = malloc (sizeof(*node)); */
      /* node->state = proc; */
      /* node->state = proc; */
      /* proc->node.state = proc; */

      task_set_state(proc->task, TASK_TRANSITION_TO_WAITING);

      queue_impl_enqueue(mutex->wait_queue, proc);
      /* mpscq_push(&mutex->wait_queue, proc); */

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
  assert (mutex->count < 100);

  DEBUG_LOG(2, "%p unlocks mutex %p\n", proc, mutex);

  if (__atomic_sub_fetch(&mutex->count, 1, __ATOMIC_SEQ_CST) > 0)
    {
      DEBUG_LOG(2, "%p found another waiting on mutex %p\n", proc, mutex);
      
      /* mpscq_node_t* node; */
      processor_t other_proc;

      /* do */
      /*   { */
      /*     other_proc = mpscq_pop(&mutex->wait_queue); */
      /*   } while(other_proc == 0); */

      /*  /\* = (processor_t) node->state; *\/ */

      queue_impl_dequeue(mutex->wait_queue, (void**)&other_proc);

      while (other_proc->task->state != TASK_WAITING);

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
