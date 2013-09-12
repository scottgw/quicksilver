#include <stdlib.h>
#include <stdint.h>
#include <ucontext.h>
#include <assert.h>

#include "libqs/types.h"
#include "libqs/processor.h"
#include "libqs/notifier.h"
#include "libqs/closure.h"
#include "libqs/private_queue.h"
#include "libqs/sync_ops.h"
#include "libqs/sched_task.h"

#include "internal/qoq.h"
#include "internal/bounded_queue.h"
#include "internal/debug_log.h"
#include "internal/executor.h"
#include "internal/task.h"
#include "internal/task_mutex.h"
#include "internal/task_condition.h"


static
void
notify_available(processor_t proc)
{

  // If we're processing a wait condition, then we don't want to
  // notify any other threads that this processor is available,
  // because here availability means:
  //
  //   This processor might have undergone a state change so
  //   check your wait-condition again.
  //
  // This is designed to prevent wait conditions from waking up other
  // wait conditions.
  if (!proc->processing_wait)
    {
      task_mutex_lock(proc->mutex, &proc->stask);
      proc->last_waiter = NULL;
      task_condition_signal_all(proc->cv, &proc->stask);
      task_mutex_unlock(proc->mutex, &proc->stask);
    }

  proc->processing_wait = false;
}

static
void
proc_duty_loop(processor_t proc, priv_queue_t priv_queue)
{
  while (true)
    {
      closure_t clos = priv_dequeue(priv_queue, proc);

      if (clos == NULL)
        {
          notify_available(proc);
          break;
        }
      else if (closure_is_end(clos))
        {
          // The end of a private queue decrements the ref_count again.
          // This should have been incremented initially when the private
          // queue was bound to this processor.
          notify_available(proc);
          free(clos);
          break;
        }
      else if (closure_is_sync(clos))
        {
          processor_t client = (processor_t) clos->fn;
          while (client->stask.task->state != TASK_WAITING);
          proc->stask.task->state = TASK_TRANSITION_TO_WAITING;

          stask_switch(&proc->stask, &client->stask);
          /* proc_wake(client, proc->executor); */
        }
      else
        {
          closure_apply(clos, NULL);
          closure_free(clos);
        }
    }
}

static
void
proc_loop(processor_t proc)
{
  stask_step_previous(&proc->stask);

  while (true)
    {
      priv_queue_t priv_queue;

      proc_maybe_yield(proc);

      // Dequeue a private queue from the queue of queues.
      qoq_dequeue_wait(proc->qoq, (void**)&priv_queue, &proc->stask);
      if (priv_queue == NULL)
        {
          return;
        }
      else
        {
          proc_duty_loop(proc, priv_queue);
        }
    }
}

priv_queue_t
proc_get_queue(processor_t proc, processor_t supplier_proc)
{
  assert (proc != NULL);
  assert (supplier_proc != NULL);

  priv_queue_t q = g_hash_table_lookup(proc->privq_cache, supplier_proc);

  if (q == NULL)
    {
      q = priv_queue_new(supplier_proc);
      g_hash_table_insert(proc->privq_cache, supplier_proc, q);
    }

  return q;
}

void
proc_maybe_yield(processor_t proc)
{
  if (time_is_up == 1)
    {
      time_is_up = 0;
      task_set_state(proc->stask.task, TASK_TRANSITION_TO_RUNNABLE);
      stask_yield_to_executor(&proc->stask);
    }
}

void
proc_wait_for_available(processor_t waitee, processor_t waiter)
{
  task_mutex_lock(waitee->mutex, &waiter->stask);
  waitee->last_waiter = waiter;

  while (waitee->last_waiter == waiter)
    {
      task_condition_wait(waitee->cv, waitee->mutex, &waiter->stask);
    }
  task_mutex_unlock(waitee->mutex, &waiter->stask);
}

void
proc_sleep(processor_t proc, struct timespec duration)
{
  sync_data_add_sleeper(proc->stask.sync_data, &proc->stask, duration);
  /* task_switch(proc->stask.task, proc->executor->stask.task); */
}


processor_t
proc_new_with_func(sync_data_t sync_data, void (*func)(processor_t))
{
  processor_t proc = (processor_t) malloc(sizeof(struct processor));

  proc->qoq = qoq_new(2048);
  stask_init(&proc->stask, sync_data, true);

  proc->available = true;
  proc->mutex = task_mutex_new();
  proc->cv = task_condition_new();

  proc->processing_wait = false;

  proc->privq_cache = g_hash_table_new(NULL, NULL);

  stask_set_func(&proc->stask, (void (*)(void*))func, proc);

  sync_data_enqueue_runnable(sync_data, &proc->stask);

  return proc;
}

processor_t
proc_new(sync_data_t sync_data)
{
  return proc_new_with_func (sync_data, proc_loop);
}

processor_t
proc_new_from_other(processor_t other_proc)
{
  return proc_new(other_proc->stask.sync_data);
}

processor_t
proc_new_root(sync_data_t sync_data, void (*root)(processor_t))
{
  return proc_new_with_func (sync_data, root);
}

void
proc_shutdown(processor_t proc, processor_t wait_proc)
{
  /* priv_queue_t shutdown_q = priv_queue_new(wait_proc); */
  /* shutdown_q->shutdown = true; */

  qoq_enqueue_wait(proc->qoq, NULL, &wait_proc->stask);
}

void
proc_free(processor_t proc)
{
  task_condition_free(proc->cv);
  task_mutex_free(proc->mutex);
  qoq_free(proc->qoq);
  g_hash_table_destroy(proc->privq_cache);

  free (proc);
}
