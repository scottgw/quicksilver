#include <stdlib.h>

#include "libqs/bounded_queue.h"
#include "libqs/queue_impl.h"
#include "libqs/task_mutex.h"
#include "libqs/task_condition.h"


struct bounded_queue
{
  task_mutex_t mutex;
  task_condition_t event;
  queue_impl_t impl;
};

bounded_queue_t
bqueue_new(uint32_t size)
{
  bounded_queue_t q = (bounded_queue_t)malloc(sizeof(struct bounded_queue));

  q->mutex = task_mutex_new();
  q->event = task_condition_new();
  q->impl  = queue_impl_new(size);

  return q;
}

void
bqueue_free(bounded_queue_t q)
{
  task_mutex_free(q->mutex);
  task_condition_free(q->event);
  queue_impl_free(q->impl);
  free(q);
}

void
bqueue_use(bounded_queue_t q)
{
  queue_impl_use(q->impl);
}

// FIXME: unused, remove?
bool
bqueue_enqueue(bounded_queue_t q, void *data, processor_t proc)
{
  
  if (queue_impl_enqueue(q->impl, data))
    {
      task_mutex_lock(q->mutex, proc);
      task_condition_signal(q->event);
      task_mutex_unlock(q->mutex, proc);
      return true;
    }
  else
    {
      return false;
    }
}

// FIXME: unused, remove?
bool
bqueue_dequeue(bounded_queue_t q, void **data)
{
  return queue_impl_dequeue(q->impl, data);
}

void
bqueue_enqueue_wait(bounded_queue_t q, void *data, processor_t proc)
{
  if (!queue_impl_enqueue(q->impl, data))
    {
      task_mutex_lock(q->mutex, proc);
      while (!queue_impl_enqueue(q->impl, data))
        {
          task_condition_wait(q->event, q->mutex, proc);
        }
      task_mutex_unlock(q->mutex, proc);
    }

  task_condition_signal(q->event);
}


void
bqueue_dequeue_wait(bounded_queue_t q, void **data, processor_t proc)
{
  if (!queue_impl_dequeue(q->impl, data))
    {
      task_mutex_lock(q->mutex, proc);
      while (!queue_impl_dequeue(q->impl, data))
        {
          task_condition_wait(q->event, q->mutex, proc);
        }
      task_mutex_unlock(q->mutex, proc);
    }
  
  task_condition_signal(q->event);
}
