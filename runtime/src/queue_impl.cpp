#include <stdlib.h>
#include <tbb/concurrent_queue.h>

#include "internal/queue_impl.h"

extern "C"
{
  struct queue_impl
  {
    tbb::concurrent_bounded_queue<void*> *impl;
  };

  queue_impl_t
  queue_impl_new(size_t size)
  {
    queue_impl_t q = (queue_impl_t) malloc(sizeof(struct queue_impl));
    q->impl = new tbb::concurrent_bounded_queue<void*>();
    q->impl->set_capacity(size);
    return q;
  }

  void
  queue_impl_free(queue_impl_t q)
  {
    delete q->impl;
    free(q);
  }

  void
  queue_impl_use(queue_impl_t q)
  {
    return;
  }

  int
  queue_impl_size(queue_impl_t q)
  {
    return q->impl->size();
  }

  bool
  queue_impl_enqueue(queue_impl_t q, void* data)
  {
    return q->impl->try_push(data);
  }

  bool
  queue_impl_dequeue(queue_impl_t q, void** data_out)
  {
    void* data;
    bool success = q->impl->try_pop(data);
    *data_out = data;
    return success;
  }

  void
  queue_impl_enqueue_wait(queue_impl_t q, void* data)
  {
    q->impl->push(data);
  }

  void
  queue_impl_dequeue_wait(queue_impl_t q, void** data_out)
  {
    void *data;
    q->impl->pop(data);
    *data_out = data;
  }

  queue_impl_t
  queue_impl_filter_out(queue_impl_t q, bool (*pred)(void*, void*), void *user)
  {
    auto sat_pred = tbb::concurrent_queue<void*>();
    auto unsat_pred = tbb::concurrent_queue<void*>();

    void* elem;

    while (queue_impl_dequeue(q, &elem))
      {
        if (pred(elem, user))
          {
            sat_pred.push(elem);
          }
        else
          {
            unsat_pred.push(elem);
          }
      }

    auto filtered = queue_impl_new(unsat_pred.unsafe_size()+1);
    
    while (unsat_pred.try_pop(elem))
      queue_impl_enqueue(filtered, elem);

    while (sat_pred.try_pop(elem))
      queue_impl_enqueue(q, elem);

    return filtered;
  }  
}

