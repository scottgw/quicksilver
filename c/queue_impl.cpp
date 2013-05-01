#include <stdlib.h>
#include <tbb/concurrent_queue.h>

#include "queue_impl.h"

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
  
}

