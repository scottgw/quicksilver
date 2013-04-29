#include <ffi.h>
#include "bounded_queue.h"
#include "closure.h"
#include "private_queue.h"
#include "processor.h"

struct priv_queue
{
  bounded_queue_t q;
  bool last_was_func;
};

priv_queue_t
priv_queue_new(processor_t proc)
{
  priv_queue_t pq = (priv_queue_t) malloc(sizeof(struct priv_queue));
  pq->last_was_func = false;
  pq->q = bqueue_new(25000);
  return pq;
}

void
priv_queue_free(priv_queue_t pq)
{
  bqueue_free(pq->q);
  free(pq);
}

void
priv_queue_shutdown(priv_queue_t pq, processor_t proc)
{
  priv_queue_lock(pq, proc);
  bqueue_enqueue(pq->q, closure_new_end());
  priv_queue_unlock(pq);
}

void
priv_queue_lock(priv_queue_t pq, processor_t proc)
{
  enqueue_private_queue(proc, pq);
}

void
priv_queue_unlock(priv_queue_t pq)
{
  enqueue_closure(pq->q, NULL);
}


closure_t
priv_dequeue(priv_queue_t pq, processor_t proc)
{
  closure_t clos;
  bqueue_dequeue_wait(pq->q, (void**)&clos, proc);
  return clos;
}


void
priv_queue_routine(priv_queue_t pq, closure_t clos)
{
  pq->last_was_func = false;
  bqueue_enqueue(pq->q, clos);
}

static
void
function_wrapper(bounded_queue_t future, closure_t clos, void* res)
{
  closure_apply(clos, res);
  bqueue_enqueue(future, res);
}

void
priv_queue_function(priv_queue_t pq,
                    closure_t clos,
                    void* res,
                    processor_t proc)
{
  if (!pq->last_was_func)
    {
      pq->last_was_func = true;
      bounded_queue_t future = bqueue_new(1);

      void ***args;
      clos_type_t *arg_types;

      closure_t promise_clos =
        closure_new(function_wrapper,
                    closure_void_type(),
                    3,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      arg_types[1] = closure_pointer_type();
      arg_types[2] = closure_pointer_type();

      *args[0] = future;
      *args[1] = clos;
      *args[2] = res;

      enqueue_closure(pq->q, promise_clos);

      // Wait for the other thread to get a value back to us.
      bqueue_dequeue_wait(future, &res, proc);

      bqueue_free(future);
    }
  else
    {
      closure_apply(clos, res);
    }
}
