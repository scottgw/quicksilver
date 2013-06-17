#include <assert.h>
#include <ffi.h>
#include <stdlib.h>

#include "libqs/bounded_queue.h"
#include "libqs/closure.h"
#include "libqs/debug_log.h"
#include "libqs/private_queue.h"
#include "libqs/processor.h"
#include "libqs/spsc_queue.h"
#include "libqs/task.h"

struct priv_queue
{
  spsc_queue_t q;
  closure_t last;

  processor_t supplier_proc;

  bool last_was_func;
};

priv_queue_t
priv_queue_new(processor_t proc)
{
  priv_queue_t pq = (priv_queue_t) malloc(sizeof(struct priv_queue));

  pq->last_was_func = false;
  pq->last = NULL;
  pq->q = spsc_new(2048);
  pq->supplier_proc = proc;

  __sync_fetch_and_add(&proc->ref_count, 1);

  return pq;
}

void
priv_queue_free(priv_queue_t pq)
{
  if (pq->last != NULL)
    {
      closure_free(pq->last);
    }

  spsc_free(pq->q);

  free(pq);
}

void
priv_queue_shutdown(priv_queue_t pq, processor_t client)
{
  priv_queue_lock(pq, client);
  spsc_enqueue_wait(pq->q, closure_new_end(), client);
  priv_queue_unlock(pq, client);
}

void
priv_queue_lock(priv_queue_t pq, processor_t client)
{
  pq->last_was_func = false;
  bqueue_enqueue_wait(pq->supplier_proc->qoq, pq, client);
}

void
priv_queue_unlock(priv_queue_t pq, processor_t client)
{
  pq->last_was_func = false;
  spsc_enqueue_wait(pq->q, NULL, client);
}


closure_t
priv_dequeue(priv_queue_t pq, processor_t proc)
{
  closure_t clos;
  spsc_dequeue_wait(pq->q, (void**)&clos, proc);
  return clos;
}


static
void
priv_queue_link_enqueue(priv_queue_t pq, closure_t clos, processor_t wait_proc)
{
  closure_t last = pq->last;
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
          if (!closure_is_sync(last))
            {
              closure_free(last);
            }
          DEBUG_LOG(1, "%p priv enqueue start\n", wait_proc);
          spsc_enqueue_wait(pq->q, clos, wait_proc);
          DEBUG_LOG(1, "%p priv enqueue end\n", wait_proc);
        }
    }
  else
    {
      DEBUG_LOG(1, "%p priv enqueue start\n", wait_proc);
      spsc_enqueue_wait(pq->q, clos, wait_proc);
      DEBUG_LOG(1, "%p priv enqueue end\n", wait_proc);
    }
}

bool
priv_queue_last_was_func(priv_queue_t pq)
{
  return pq->last_was_func;
}

void
priv_queue_routine(priv_queue_t pq, closure_t clos, processor_t wait_proc)
{
  pq->last_was_func = false;
  priv_queue_link_enqueue(pq, clos, wait_proc);
}

void
priv_queue_lock_sync(priv_queue_t pq, processor_t client)
{
  /* struct closure sync_clos; */
  closure_t sync_clos = malloc(sizeof(*sync_clos));
  closure_new_sync(sync_clos, client);
  pq->last = NULL;

  priv_queue_link_enqueue(pq, sync_clos, client);

  bqueue_enqueue_wait(pq->supplier_proc->qoq, pq, client);

  task_set_state(client->task, TASK_TRANSITION_TO_WAITING);
  proc_yield_to_executor(client);

  pq->last_was_func = true;
}


void
priv_queue_sync(priv_queue_t pq, processor_t client)
{
  if (!pq->last_was_func)
    {
      /* struct closure sync_clos; */
      closure_t sync_clos = malloc(sizeof(*sync_clos));
      closure_new_sync(sync_clos, client);
      pq->last = NULL;

      priv_queue_link_enqueue(pq, sync_clos, client);

      task_set_state(client->task, TASK_TRANSITION_TO_WAITING);
      proc_yield_to_executor(client);
    }
  pq->last_was_func = true;
}
