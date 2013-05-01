#include "liblfds611.h"

#include "bounded_queue.h"
#include "queue_impl.h"
#include "task_mutex.h"
#include "task_condition.h"


struct bounded_queue
{
  volatile uint32_t n;
  task_mutex_t mutex;
  task_condition_t event;
  queue_impl_t impl;
};

bounded_queue_t
bqueue_new(uint32_t size)
{
  bounded_queue_t q = (bounded_queue_t)malloc(sizeof(struct bounded_queue));

  q->n = 0;
  q->mutex = task_mutex_new();
  q->event = task_condition_new();
  q->impl = queue_impl_new(size);

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

bool
bqueue_enqueue(bounded_queue_t q, void *data)
{
  if (queue_impl_enqueue(q->impl, data))
    {
      __sync_fetch_and_add(&q->n, 1);
      task_condition_signal(q->event);
      return true;
    }
  else
    {
      return false;
    }
}


void
bqueue_enqueue_wait(bounded_queue_t q, void *data, processor_t proc)
{
  for (int i = 0; i < 1024; i++)
    {
      if (queue_impl_enqueue(q->impl, data))
        {
          __sync_fetch_and_add(&q->n, 1);
          task_condition_signal(q->event);
          return;
        }
    }

  if (!queue_impl_enqueue(q->impl, data))
    {
      task_mutex_lock(q->mutex, proc);
      while (!queue_impl_enqueue(q->impl, data))
        {
          task_condition_wait(q->event, q->mutex, proc);
        }
      task_mutex_unlock(q->mutex, proc);
    }

  __sync_fetch_and_add(&q->n, 1);
  task_condition_signal(q->event);
}


bool
bqueue_dequeue(bounded_queue_t q, void **data)
{
  return queue_impl_dequeue(q->impl, data);
}


void
bqueue_dequeue_wait(bounded_queue_t q, void **data, processor_t proc)
{
  for (int i = 0; i < 1024; i++)
    {
      if (queue_impl_dequeue(q->impl, data))
        {
          __sync_fetch_and_sub(&q->n, 1);
          task_condition_signal(q->event);
          return;
        }
    }

  if (!queue_impl_dequeue(q->impl, data))
    {
      task_mutex_lock(q->mutex, proc);
      while (!queue_impl_dequeue(q->impl, data))
        {
          task_condition_wait(q->event, q->mutex, proc);
        }
      task_mutex_unlock(q->mutex, proc);
    }

  __sync_fetch_and_sub(&q->n, 1);
  task_condition_signal(q->event);
}
