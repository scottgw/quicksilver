#include <assert.h>
#include <ffi.h>
#include <stdlib.h>

#include "libqs/bounded_queue.h"
#include "libqs/mpsc_blocking.h"
#include "libqs/closure.h"
#include "libqs/debug_log.h"
#include "libqs/private_queue.h"
#include "libqs/processor.h"
#include "libqs/spsc_queue.h"
#include "libqs/task.h"

priv_queue_t
priv_queue_new(processor_t proc)
{
  priv_queue_t pq = (priv_queue_t) malloc(sizeof(struct priv_queue));

  pq->last_was_func = false;
  pq->last = NULL;
  pq->q = spsc_new(2048);
  pq->supplier_proc = proc;
  pq->shutdown = false;

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


static
void
priv_queue_resume_supplier(priv_queue_t pq, processor_t client)
{
  if (pq->last_was_func)
    {
      proc_wake(pq->supplier_proc, client->executor);
    }
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
  qo_q_enqueue_wait(pq->supplier_proc->qoq, pq, client);
}

void
priv_queue_unlock(priv_queue_t pq, processor_t client)
{
  priv_queue_resume_supplier(pq, client);
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

bool
priv_queue_last_was_func(priv_queue_t pq)
{
  return pq->last_was_func;
}

void
priv_queue_routine(priv_queue_t pq, closure_t clos, processor_t wait_proc)
{
  spsc_enqueue_wait(pq->q, clos, wait_proc);
  priv_queue_resume_supplier(pq, wait_proc);
  pq->last_was_func = false;
}

void
priv_queue_lock_sync(priv_queue_t pq, processor_t client)
{
  /* struct closure sync_clos; */
  closure_t sync_clos = malloc(sizeof(*sync_clos));
  closure_new_sync(sync_clos, client);
  pq->last = NULL;

  spsc_enqueue_wait(pq->q, sync_clos, client);
  assert (client->task->state == TASK_RUNNING);
  qo_q_enqueue_wait(pq->supplier_proc->qoq, pq, client);

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
      /* closure_t sync_clos = malloc(sizeof(*sync_clos)); */
      struct closure sync_clos;
      closure_new_sync(&sync_clos, client);
      pq->last = NULL;

      spsc_enqueue_wait(pq->q, &sync_clos, client);

      task_set_state(client->task, TASK_TRANSITION_TO_WAITING);
      proc_yield_to_executor(client);
    }
  pq->last_was_func = true;
}
