#include <assert.h>
#include <ffi.h>
#include <stdlib.h>
#include "internal/bounded_queue.h"
#include "internal/qoq.h"
#include "libqs/closure.h"
#include "internal/debug_log.h"
#include "libqs/private_queue.h"
#include "libqs/processor.h"
#include "libqs/sched_task.h"
#include "internal/task.h"
#include "internal/task_mutex.h"
#include "internal/task_condition.h"
#include "internal/spsc_queue.h"

priv_queue_t
priv_queue_new(processor_t proc)
{
  priv_queue_t pq = (priv_queue_t) malloc(sizeof(struct priv_queue));

  pq->last_was_func = false;
  pq->sync_check_enabled = true;
  pq->q = spsc_new();
  pq->supplier_proc = proc;
  pq->shutdown = false;

  return pq;
}

void
priv_queue_free(priv_queue_t pq)
{
  spsc_free(pq->q);

  free(pq);
}

void
priv_queue_set_in_wait(priv_queue_t pq)
{
  pq->supplier_proc->processing_wait = true;
}

void
priv_queue_set_in_body(priv_queue_t pq)
{
  pq->supplier_proc->processing_wait = false;
}


static
void
priv_queue_resume_supplier(priv_queue_t pq, processor_t client)
{
  if (pq->last_was_func)
    {
      stask_wake(&pq->supplier_proc->stask, client->stask.executor);
    }
}

void
priv_queue_lock(priv_queue_t pq, processor_t client)
{
  pq->last_was_func = false;
  qoq_enqueue_wait(pq->supplier_proc->qoq, pq, &client->stask);
}

void
priv_queue_unlock(priv_queue_t pq, processor_t client)
{
  priv_queue_resume_supplier(pq, client);
  pq->last_was_func = false;
  spsc_enqueue_wait(pq->q, NULL, &client->stask);
}

bool
priv_queue_last_was_func(priv_queue_t pq)
{
  return pq->last_was_func;
}

void
priv_queue_routine(priv_queue_t pq, closure_t clos, processor_t wait_proc)
{
  spsc_enqueue_wait(pq->q, clos, &wait_proc->stask);
  priv_queue_resume_supplier(pq, wait_proc);
  pq->last_was_func = false;
}

closure_t
priv_dequeue(priv_queue_t pq, processor_t proc)
{
  closure_t clos;
  spsc_dequeue_wait(pq->q, (void**)&clos, &proc->stask);
  return clos;
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

      spsc_enqueue_wait(pq->q, &sync_clos, &client->stask);

      task_set_state(client->stask.task, TASK_TRANSITION_TO_WAITING);
      stask_yield_to_executor(&client->stask);
    }

  pq->last_was_func = true;
}
