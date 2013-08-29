#include <assert.h>
#include <stdlib.h>

#include "internal/queue_impl.h"
#include "internal/sched_task.h"
#include "internal/task.h"
#include "internal/task_mutex.h"


struct task_mutex 
{
  uint32_t count;
  sched_task_t owner;

  volatile int inner;

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
      c = 0;
      __atomic_compare_exchange_4(&mutex->inner, &c, 1,
                                  false, __ATOMIC_SEQ_CST,
                                  __ATOMIC_SEQ_CST);

      if(!c)
        {
          return;
        }
    }

  if (c == 1)
    {
      c = __atomic_exchange_4(&mutex->inner, 2, __ATOMIC_SEQ_CST);
    }

  while (c)
    {
      // set the state to locked and contended, and sleep if it WAS locked
      // before the exchange.
      if(__atomic_load_4(&mutex->inner, __ATOMIC_SEQ_CST) == 2)
        {
          task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);
          queue_impl_enqueue(mutex->wait_queue, stask);
          stask_yield_to_executor(stask);
        }
      c = __atomic_exchange_4(&mutex->inner, 2, __ATOMIC_SEQ_CST);
    }

  /* mutex->owner = stask; */
}

void
task_mutex_unlock(volatile task_mutex_t mutex, sched_task_t stask)
{
  if (__atomic_load_4(&mutex->inner, __ATOMIC_SEQ_CST) == 2)
    {
      __atomic_store_4(&mutex->inner, 0, __ATOMIC_SEQ_CST);
    }
  else if (__atomic_exchange_4(&mutex->inner, 0, __ATOMIC_SEQ_CST) == 1)
    {
      return;
    }


  for (int i = 0; i < 200; i++)
    {
      if (__atomic_load_4(&mutex->inner, __ATOMIC_SEQ_CST))
        {
          int old = 1;
          __atomic_compare_exchange_4(&mutex->inner, &old, 2,
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

    if (queue_impl_dequeue(mutex->wait_queue, (void**)&other_stask))
      {
        stask_wake(other_stask, stask->executor);
      }
  }

  return;
}
