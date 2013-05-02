#include <ffi.h>
#include "bounded_queue.h"
#include "closure.h"
#include "private_queue.h"
#include "processor.h"

struct priv_queue
{
  bounded_queue_t q;
  closure_t last;

  bool last_was_func;
};

priv_queue_t
priv_queue_new(processor_t proc)
{
  priv_queue_t pq = (priv_queue_t) malloc(sizeof(struct priv_queue));
  pq->last_was_func = false;
  pq->last = NULL;
  pq->q = bqueue_new(2048);
  return pq;
}

void
priv_queue_free(priv_queue_t pq)
{
  if (pq->last != NULL)
    closure_free(pq->last);
  bqueue_free(pq->q);
  free(pq);
}

void
priv_queue_shutdown(priv_queue_t pq, processor_t proc, processor_t wait_proc)
{
  priv_queue_lock(pq, proc, wait_proc);
  enqueue_closure(pq->q, closure_new_end(), proc);
  priv_queue_unlock(pq, proc);
}

void
priv_queue_lock(priv_queue_t pq, processor_t proc, processor_t wait_proc)
{
  enqueue_private_queue(proc, pq, wait_proc);
}

void
priv_queue_unlock(priv_queue_t pq, processor_t proc)
{
  enqueue_closure(pq->q, NULL, proc);
}


closure_t
priv_dequeue(priv_queue_t pq, processor_t proc)
{
  return dequeue_closure(pq->q, proc);
}


void
priv_queue_routine(priv_queue_t pq, closure_t clos, processor_t wait_proc)
{
  closure_t last = pq->last;
  pq->last_was_func = false;
  pq->last = clos;

  if (last != NULL)
    {
      if (__sync_bool_compare_and_swap(&last->next, NULL, clos))
        {
          // If the other closure hasn't finished yet, we don't do anything.
        }
      else
        {
          // If we couldn't swap this closure in, queue it up.
          closure_free(last);
          enqueue_closure(pq->q, clos, wait_proc);
        }
    }
  else
    {
      enqueue_closure(pq->q, clos, wait_proc);  
    }
}

static
void
function_wrapper(bounded_queue_t future, closure_t clos, void* res, processor_t proc)
{
  closure_apply(clos, res);
  bqueue_enqueue_wait(future, res, proc);
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
                    4,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      arg_types[1] = closure_pointer_type();
      arg_types[2] = closure_pointer_type();
      arg_types[3] = closure_pointer_type();

      *args[0] = future;
      *args[1] = clos;
      *args[2] = res;
      *args[3] = proc;

      enqueue_closure(pq->q, promise_clos, proc);

      // Wait for the other thread to get a value back to us.
      bqueue_dequeue_wait(future, &res, proc);

      bqueue_free(future);
    }
  else
    {
      closure_apply(clos, res);
    }
}
