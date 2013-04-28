#ifndef _PRIVATE_QUEUE_H
#define _PRIVATE_QUEUE_H

#include "closure.h"

#include "types.h"

priv_queue_t
priv_queue_new(processor_t proc);

void
priv_queue_free(priv_queue_t q);

void
priv_queue_unlock(priv_queue_t q);

void
priv_queue_routine(priv_queue_t q, closure_t clos);

void
priv_queue_function(priv_queue_t pq,
                    closure_t clos,
                    void* res,
                    processor_t proc);
#endif // _PRIVATE_QUEUE_H
