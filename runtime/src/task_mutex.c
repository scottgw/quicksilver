
#include <assert.h>
#include <pthread.h>
#include <stdlib.h>

#include "internal/queue_impl.h"
#include "libqs/sched_task.h"
#include "internal/task.h"
#include "internal/task_mutex.h"

#define CAS(ptr, expected, desired) \
  __atomic_compare_exchange(&ptr, &expected, &desired,	 \
			    false,			 \
			    __ATOMIC_SEQ_CST,		 \
			    __ATOMIC_SEQ_CST);		 \

#define XCHG(destination, newval, previous) \
  __atomic_exchange(&destination, &newval, &previous, __ATOMIC_SEQ_CST);

struct task_mutex 
{
  uint32_t count;
  sched_task_t owner;

  volatile int32_t inner;
  pthread_spinlock_t spinlock;

  queue_impl_t wait_queue;
};

task_mutex_t
task_mutex_new()
{
  task_mutex_t mutex = (task_mutex_t)malloc(sizeof(struct task_mutex));

  mutex->inner = 0;
  mutex->wait_queue = queue_impl_new(20000);
  pthread_spin_init(&mutex->spinlock, 0);  

  return mutex;
}

void
task_mutex_free(task_mutex_t mutex)
{
  queue_impl_free(mutex->wait_queue);
  pthread_spin_destroy(&mutex->spinlock);
  free (mutex);
}

sched_task_t
task_mutex_owner(task_mutex_t mutex)
{
  return mutex->owner;
}

#define SPIN_COUNT 8

#define EMPTY 0
#define SINGLE 1
#define MULTIPLE 2

void
task_mutex_lock(task_mutex_t mutex, volatile sched_task_t stask)
{
  int c;

  for (int i = 0; i < SPIN_COUNT; i++)
    {
      int32_t desired = SINGLE;
      c = EMPTY;
      CAS(mutex->inner, c, desired);

      if(c == EMPTY)
        {
          mutex->owner = stask;
          return;
        }
    }

  if (c == SINGLE)
    {
      int newval = MULTIPLE;
      XCHG(mutex->inner, newval, c);
    }

  while (c != EMPTY)
    {
      // set the state to locked and contended, and sleep if it WAS locked
      // before the exchange.

      {
        pthread_spin_lock (&mutex->spinlock);
	int inner;
	__atomic_load(&mutex->inner, &inner, __ATOMIC_SEQ_CST);
	if(inner == MULTIPLE)
	  {
	    task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);
	    queue_impl_enqueue(mutex->wait_queue, stask);
            pthread_spin_unlock (&mutex->spinlock);
	    stask_yield_to_executor(stask);
	  }
        pthread_spin_unlock (&mutex->spinlock);
      }
      int desired = MULTIPLE;
      XCHG (mutex->inner, desired, c);
    }

  mutex->owner = stask;
}

void
task_mutex_unlock(volatile task_mutex_t mutex, sched_task_t stask)
{
  int inner;
  __atomic_load(&mutex->inner, &inner, __ATOMIC_SEQ_CST);
  if (inner == MULTIPLE)
    {
      inner = EMPTY;
      __atomic_store(&mutex->inner, &inner, __ATOMIC_SEQ_CST);
    }
  else
    {
      int newval = EMPTY;
      XCHG (mutex->inner, newval, inner);
      if (inner == 1)
        {
          return;
        }
    }


  for (int i = 0; i < SPIN_COUNT; i++)
    {
      __atomic_load(&mutex->inner, &inner, __ATOMIC_SEQ_CST);
      if (inner != EMPTY)
        {
          int expected_old = SINGLE;
          int desired = MULTIPLE;
          CAS (mutex->inner, expected_old, desired);
          if (expected_old != EMPTY)
            {
              return;
            }
        }
    }

  {
    pthread_spin_lock (&mutex->spinlock);
    // pop a task if it exists, and resume it.
    sched_task_t other_stask;
    if (queue_impl_dequeue(mutex->wait_queue, (void**)&other_stask))
      {
        stask_wake(other_stask, stask->executor);
      }
    pthread_spin_unlock (&mutex->spinlock);
  }

  return;
}

