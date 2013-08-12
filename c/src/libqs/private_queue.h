#ifndef _PRIVATE_QUEUE_H
#define _PRIVATE_QUEUE_H

#include "closure.h"
#include "types.h"

struct priv_queue
{
  spsc_queue_t q;
  closure_t last;

  processor_t supplier_proc;

  bool last_was_func;
  bool shutdown;

  struct priv_queue *next;
};


priv_queue_t
priv_queue_new(processor_t proc);

void
priv_queue_free(priv_queue_t q);

void
priv_queue_shutdown(priv_queue_t q, processor_t wait_proc);

bool
priv_queue_last_was_func(priv_queue_t pq);

void
priv_queue_set_in_wait(priv_queue_t pq);

void
priv_queue_set_in_body(priv_queue_t pq);

void
priv_queue_lock(priv_queue_t q, processor_t wait_proc);

void
priv_queue_unlock(priv_queue_t q, processor_t proc);

closure_t
priv_dequeue(priv_queue_t q, processor_t proc);

void
priv_queue_routine(priv_queue_t q, closure_t clos, processor_t wait_proc);

/* void */
/* access_wrapper(processor_t proc, clos_type_t type, void* source, void *target); */

/* void */
/* function_wrapper(closure_t clos, void* res, processor_t proc); */

/* void */
/* priv_queue_function(priv_queue_t pq, */
/*                     closure_t clos, */
/*                     void* res, */
/*                     processor_t proc); */


void
priv_queue_lock_sync(priv_queue_t pq, processor_t client);


void
priv_queue_sync(priv_queue_t pq, processor_t client);

#endif // _PRIVATE_QUEUE_H
