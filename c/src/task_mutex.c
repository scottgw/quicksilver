#include <assert.h>
#include <stdlib.h>
#include <linux/sched.h>

#include "internal/debug_log.h"
#include "internal/mpscq.h"
#include "internal/sched_task.h"
#include "internal/task.h"
#include "internal/task_mutex.h"

#define cpu_relax() \
  __asm__ __volatile__ ( "pause" : : : "memory" )

#define INIT_WAIT_QUEUE_SIZE 16000

union mutex_inner
{
  unsigned u;
  struct
  {
    unsigned char locked; // this is the low byte
    unsigned char contended;
  } b;
};

typedef union mutex_inner mutex_inner;

struct task_mutex 
{
  uint32_t count;
  sched_task_t owner;
  /* struct mpscq_node_t stub; */

  volatile mutex_inner *inner;

  mpscq_t wait_queue;
};

task_mutex_t
task_mutex_new()
{
  task_mutex_t mutex = (task_mutex_t)malloc(sizeof(struct task_mutex));

  mutex->count = 0;
  mutex->owner = NULL;
  mutex->inner = (mutex_inner*) malloc(sizeof(mutex_inner));
  mutex->inner->u = 0;

  mpscq_create(&mutex->wait_queue, NULL); // mutex->stub);

  return mutex;
}

void
task_mutex_free(task_mutex_t mutex)
{
  free ((void*)mutex->inner);
  free (mutex);
}

sched_task_t
task_mutex_owner(task_mutex_t mutex)
{
  return NULL; //  mutex->owner;
}

void
task_mutex_lock(task_mutex_t mutex, volatile sched_task_t stask)
{
  for (int i = 0; i < 100; i++)
    {
      if (__atomic_exchange_1(&mutex->inner->b.locked, 1, __ATOMIC_SEQ_CST) == 0)
        {
          return;
        }

      cpu_relax();
    }

  while (__atomic_exchange_4(&mutex->inner->u, 257, __ATOMIC_SEQ_CST) & 1)
    {
      // set the state to locked and contended, and sleep if it WAS locked
      // before the exchange.
      if(mutex->inner->u == 257)
        {
          task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);
          mpscq_push(&mutex->wait_queue, stask);
          stask_yield_to_executor(stask);
        }
    }

  /* mutex->owner = stask; */
}

void
task_mutex_unlock(volatile task_mutex_t mutex, sched_task_t stask)
{
  /* assert(mutex->owner == stask); */
  /* assert(mutex->count > 0); */

  unsigned expected = 1;

  if ((mutex->inner->u == 1) &&
      (__atomic_compare_exchange_4(&mutex->inner->u, &expected, 0,
                                   false, __ATOMIC_SEQ_CST, __ATOMIC_SEQ_CST)))
    {
      // if it was locked (but not contended), then try to switch it to unlocked.
      // If it worked, then return.
      return;
    }

  // Set it to unlocked, leaving the contention flag alone.
  __atomic_store_1(&mutex->inner->b.locked, 0, __ATOMIC_SEQ_CST);

  __atomic_thread_fence(__ATOMIC_SEQ_CST);

  for (int i = 0; i < 100; i++)
    {
      // if another thread locked it, we're done.
      if (__atomic_load_1(&mutex->inner->b.locked, __ATOMIC_SEQ_CST))
        {
          return;
        }
      cpu_relax();
    }

  // If no other thread tried, then its uncontended.
  __atomic_store_1(&mutex->inner->b.contended, 0, __ATOMIC_SEQ_CST);
  
  {
    // pop a task if it exists, and resume it.
    sched_task_t other_stask;
    other_stask = mpscq_pop(&mutex->wait_queue);
    if (other_stask != 0)
      {
        stask_wake(other_stask, stask->executor);
      }
  }

  return;
}
