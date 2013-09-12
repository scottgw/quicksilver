#include <assert.h>
#include <stdlib.h>

#include "internal/queue_impl.h"
#include "libqs/sched_task.h"
#include "internal/task.h"
#include "internal/task_mutex.h"


struct task_mutex 
{
  uint32_t count;
  sched_task_t owner;

  volatile int32_t inner;

  queue_impl_t wait_queue;
};

task_mutex_t
task_mutex_new()
{
  task_mutex_t mutex = (task_mutex_t)malloc(sizeof(struct task_mutex));

  mutex->inner = 0;
  mutex->wait_queue = queue_impl_new(20000);

  return mutex;
}

void
task_mutex_free(task_mutex_t mutex)
{
  queue_impl_free(mutex->wait_queue);
  free (mutex);
}

sched_task_t
task_mutex_owner(task_mutex_t mutex)
{
  return mutex->owner;
}

void
task_mutex_lock(task_mutex_t mutex, volatile sched_task_t stask)
{
  int c;

  for (int i = 0; i < 100; i++)
    {
      int32_t desired = 1;
      c = 0;
      __atomic_compare_exchange(&mutex->inner, &c, &desired,
                                false, __ATOMIC_SEQ_CST,
                                __ATOMIC_SEQ_CST);

      if(!c)
        {
          mutex->owner = stask;
          return;
        }
    }

  if (c == 1)
    {
      int newval = 2;
      __atomic_exchange(&mutex->inner, &newval, &c, __ATOMIC_SEQ_CST);
    }

  while (c)
    {
      // set the state to locked and contended, and sleep if it WAS locked
      // before the exchange.
      int inner;
      __atomic_load(&mutex->inner, &inner, __ATOMIC_SEQ_CST);
      if(inner == 2)
        {
          task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);
          queue_impl_enqueue(mutex->wait_queue, stask);
          stask_yield_to_executor(stask);
        }

      int desired = 2;
      __atomic_exchange(&mutex->inner, &desired, &c, __ATOMIC_SEQ_CST);
    }

  mutex->owner = stask;
}

void
task_mutex_unlock(volatile task_mutex_t mutex, sched_task_t stask)
{
  int inner;
  __atomic_load(&mutex->inner, &inner, __ATOMIC_SEQ_CST);
  if (inner == 2)
    {
      inner = 0;
      __atomic_store(&mutex->inner, &inner, __ATOMIC_SEQ_CST);
    }
  else
    {
      int newval = 0;
      __atomic_exchange(&mutex->inner, &newval, &inner, __ATOMIC_SEQ_CST);
      if (inner == 1)
        {
          return;
        }
    }


  for (int i = 0; i < 200; i++)
    {
      __atomic_load(&mutex->inner, &inner, __ATOMIC_SEQ_CST);
      if (inner)
        {
          int old = 1;
          int desired = 2;
          __atomic_compare_exchange(&mutex->inner, &old, &desired,
                                    false, __ATOMIC_SEQ_CST,
                                    __ATOMIC_SEQ_CST);
          if (old)
            {
              return;
            }
        }
    }


  {
    // pop a task if it exists, and resume it.
    sched_task_t other_stask;
    // FIXME: this is a terrible hack that greatly reduces a
    // datarace, but doesn't completely solve it.
    // 
    // Once in a while there will be a straggler left in the wait queue.
    for (int i = 0; i < 1024; i++)
      {
        if (queue_impl_dequeue(mutex->wait_queue, (void**)&other_stask))
          {
            stask_wake(other_stask, stask->executor);
            return;
          }
      }
  }

  return;
}

