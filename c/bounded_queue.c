#include "liblfds611.h"

#include "task_mutex.h"
#include "task_condition.h"
#include "bounded_queue.h"


struct bounded_queue
{
  volatile uint32_t n;
  task_mutex_t mutex;
  task_condition_t not_empty;
  struct lfds611_queue_state *impl;
};

bounded_queue_t
bqueue_new(uint32_t size)
{
  bounded_queue_t q = (bounded_queue_t)malloc(sizeof(struct bounded_queue));

  q->n = 0;
  q->mutex = task_mutex_new();
  q->not_empty = task_condition_new();

  assert(lfds611_queue_new(&q->impl, size) == 1);

  return q;
}

void
bqueue_free(bounded_queue_t q)
{
  bqueue_free_with(q, NULL, NULL);
}

void
bqueue_use(bounded_queue_t q)
{
  lfds611_queue_use(q->impl);
}

void
bqueue_free_with(bounded_queue_t q, void (*del_func)(void*, void*), void *ptr)
{
  task_mutex_free(q->mutex);
  task_condition_free(q->not_empty);
  lfds611_queue_delete(q->impl, del_func, ptr);
  free(q);
}

bool
bqueue_enqueue(bounded_queue_t q, void *data)
{
  if (__sync_fetch_and_add(&q->n, 1) == 0)
    {
      lfds611_queue_guaranteed_enqueue(q->impl, data);
      task_condition_signal(q->not_empty);
    }
  else
    {
      lfds611_queue_guaranteed_enqueue(q->impl, data);
      task_condition_signal(q->not_empty);
    }
  return true;
}


bool
bqueue_dequeue(bounded_queue_t q, void **data)
{
  return lfds611_queue_dequeue(q->impl, data);
}


void
bqueue_dequeue_wait(bounded_queue_t q, void **data, processor_t proc)
{
  if (!lfds611_queue_dequeue(q->impl, data))
    {
      task_mutex_lock(q->mutex, proc);
      while (!lfds611_queue_dequeue(q->impl, data))
        {
          task_condition_wait(q->not_empty, q->mutex, proc);
        }
      task_mutex_unlock(q->mutex, proc);
    }

  __sync_fetch_and_sub(&q->n, 1);
}
