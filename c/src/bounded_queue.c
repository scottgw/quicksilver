#include <stdlib.h>

#include "libqs/bounded_queue.h"
#include "libqs/debug_log.h"
#include "libqs/queue_impl.h"
#include "libqs/task_mutex.h"
#include "libqs/task_condition.h"

struct bounded_queue
{
  task_mutex_t not_empty_mutex;
  task_condition_t not_empty;

  task_mutex_t not_full_mutex;
  task_condition_t not_full;

  volatile uint32_t waiters;

  queue_impl_t impl;
};

bounded_queue_t
bqueue_new(uint32_t size)
{
  bounded_queue_t q = (bounded_queue_t)malloc(sizeof(struct bounded_queue));

  q->not_empty_mutex = task_mutex_new();
  q->not_empty = task_condition_new();

  q->not_full_mutex = task_mutex_new();
  q->not_full = task_condition_new();

  q->waiters = 0;

  q->impl  = queue_impl_new(size);

  return q;
}

void
bqueue_free(bounded_queue_t q)
{
  task_mutex_free(q->not_empty_mutex);
  task_condition_free(q->not_empty);

  task_mutex_free(q->not_full_mutex);
  task_condition_free(q->not_full);

  queue_impl_free(q->impl);
  free(q);
}

void
bqueue_use(bounded_queue_t q)
{
  queue_impl_use(q->impl);
}

void
bqueue_enqueue_wait(bounded_queue_t q, void *data, sched_task_t stask)
{
  for (int i = 0; i < 32; i++)
    {
      if(queue_impl_enqueue(q->impl, data))
        {
          task_mutex_lock(q->not_empty_mutex, stask);
          task_condition_signal_all(q->not_empty, stask);
          task_mutex_unlock(q->not_empty_mutex, stask);
          return;
        }
    }

  if (!queue_impl_enqueue(q->impl, data))
    {
      task_mutex_lock(q->not_full_mutex, stask);
      while (!queue_impl_enqueue(q->impl, data))
        {
          DEBUG_LOG(2, "%p waiting to enqueue in %p\n", stask, q);
          task_condition_wait(q->not_full, q->not_full_mutex, stask);
        }
      /* task_condition_signal(q->not_full, stask); */
      task_mutex_unlock(q->not_full_mutex, stask);
    }

  DEBUG_LOG(2, "%p bounded queue notifying not empty %p\n", stask, q);
  task_mutex_lock(q->not_empty_mutex, stask);
  task_condition_signal_all(q->not_empty, stask);
  task_mutex_unlock(q->not_empty_mutex, stask);
}


void
bqueue_dequeue_wait(bounded_queue_t q, void **data, sched_task_t stask)
{
  for (int i = 0; i < 32; i++)
    {
      if(queue_impl_dequeue(q->impl, data))
        {
          task_mutex_lock(q->not_full_mutex, stask);
          task_condition_signal_all(q->not_full, stask);
          task_mutex_unlock(q->not_full_mutex, stask);
          return;
        }
    }

  if (!queue_impl_dequeue(q->impl, data))
    {
      task_mutex_lock(q->not_empty_mutex, stask);
      while (!queue_impl_dequeue(q->impl, data))
        {
          DEBUG_LOG(2, "%p waiting to dequeue in %p\n", stask, q);
          task_condition_wait(q->not_empty, q->not_empty_mutex, stask);
        }
      /* task_condition_signal(q->not_empty); */
      task_mutex_unlock(q->not_empty_mutex, stask);
    }

  DEBUG_LOG(2, "%p bounded queue notifying not full %p\n", stask, q);
  task_mutex_lock(q->not_full_mutex, stask);
  task_condition_signal_all(q->not_full, stask);
  task_mutex_unlock(q->not_full_mutex, stask);
}
