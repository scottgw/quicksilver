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

/* // FIXME: unused, remove? */
/* bool */
/* bqueue_enqueue(bounded_queue_t q, void *data, processor_t proc) */
/* { */
  
/*   if (queue_impl_enqueue(q->impl, data)) */
/*     { */
/*       task_mutex_lock(q->mutex, proc); */
/*       task_condition_signal(q->event); */
/*       task_mutex_unlock(q->mutex, proc); */
/*       return true; */
/*     } */
/*   else */
/*     { */
/*       return false; */
/*     } */
/* } */

/* // FIXME: unused, remove? */
/* bool */
/* bqueue_dequeue(bounded_queue_t q, void **data) */
/* { */
/*   return queue_impl_dequeue(q->impl, data); */
/* } */

void
bqueue_enqueue_wait(bounded_queue_t q, void *data, processor_t proc)
{
  for (int i = 0; i < 512; i++)
    {
      if(queue_impl_enqueue(q->impl, data))
        {
          task_mutex_lock(q->not_empty_mutex, proc);
          task_condition_signal(q->not_empty);
          task_mutex_unlock(q->not_empty_mutex, proc);
          return;
        }
    }

  if (!queue_impl_enqueue(q->impl, data))
    {
      task_mutex_lock(q->not_full_mutex, proc);
      while (!queue_impl_enqueue(q->impl, data))
        {
          DEBUG_LOG(2, "%p waiting to enqueue in %p\n", proc, q);
          task_condition_wait(q->not_full, q->not_full_mutex, proc);
        }
      task_condition_signal(q->not_full);
      task_mutex_unlock(q->not_full_mutex, proc);
    }

  task_mutex_lock(q->not_empty_mutex, proc);
  task_condition_signal(q->not_empty);
  task_mutex_unlock(q->not_empty_mutex, proc);
}


void
bqueue_dequeue_wait(bounded_queue_t q, void **data, processor_t proc)
{
  for (int i = 0; i < 512; i++)
    {
      if(queue_impl_dequeue(q->impl, data))
        {
          task_mutex_lock(q->not_full_mutex, proc);
          task_condition_signal(q->not_full);
          task_mutex_unlock(q->not_full_mutex, proc);
          return;
        }
    }

  if (!queue_impl_dequeue(q->impl, data))
    {
      task_mutex_lock(q->not_empty_mutex, proc);
      __sync_fetch_and_add(&q->waiters, 1);
      while (!queue_impl_dequeue(q->impl, data))
        {
          DEBUG_LOG(2, "%p waiting to dequeue in %p\n", proc, q);
          task_condition_wait(q->not_empty, q->not_empty_mutex, proc);
        }
      task_condition_signal(q->not_empty);
      __sync_fetch_and_sub(&q->waiters, 1);
      task_mutex_unlock(q->not_empty_mutex, proc);
    }

  task_mutex_lock(q->not_full_mutex, proc);
  task_condition_signal(q->not_full);
  task_mutex_unlock(q->not_full_mutex, proc);
}
