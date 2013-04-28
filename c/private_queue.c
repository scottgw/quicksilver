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
  pq->q = proc_make_private_queue(proc);
  return pq;
}

void
priv_queue_free(priv_queue_t pq)
{
  free(pq);
}

void
priv_queue_unlock(priv_queue_t pq)
{
  enqueue_closure(pq->q, NULL);
}

void
priv_queue_routine(priv_queue_t pq, closure_t clos)
{
  pq->last_was_func = false;
  enqueue_closure(pq->q, clos);
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
      ffi_cif *cif = (ffi_cif*)malloc(sizeof(ffi_cif));
      ffi_type **arg_types = (ffi_type**)malloc(3*sizeof(ffi_type*));
      arg_types[0] = &ffi_type_pointer;
      arg_types[1] = &ffi_type_pointer;
      arg_types[2] = &ffi_type_pointer;
      assert
        (ffi_prep_cif
         (cif, FFI_DEFAULT_ABI, 3, &ffi_type_void, arg_types) ==
         FFI_OK);

      void **args = (void**)malloc(3*sizeof(void*));
      bounded_queue_t *future_ptr = malloc(sizeof(bounded_queue_t));
      closure_t *clos_ptr = malloc(sizeof(closure_t));
      void **res_ptr = malloc(sizeof(void*));

      *future_ptr = future;
      *clos_ptr = clos;
      *res_ptr = res;

      args[0] = future_ptr;
      args[1] = clos_ptr;
      args[2] = res_ptr;

      closure_t promise_clos = closure_new(cif, function_wrapper, 3, args);
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
