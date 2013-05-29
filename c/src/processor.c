#include <stdlib.h>
#include <stdint.h>
#include <ucontext.h>
#include <assert.h>

#include "libqs/bounded_queue.h"
#include "libqs/closure.h"
#include "libqs/debug_log.h"
#include "libqs/executor.h"
#include "libqs/maybe.h"
#include "libqs/notifier.h"
#include "libqs/processor.h"
#include "libqs/private_queue.h"
#include "libqs/task.h"

int global_id = 0;

static
void
reset_stack_to(void (*f)(void*), processor_t proc)
{
  task_set_func(proc->task, f, proc);
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
      task_set_state(proc->task, TASK_TRANSITION_TO_RUNNABLE);
      proc_yield_to_executor(proc);
    }
}

static
void
notify_available(processor_t proc)
{
  task_mutex_lock(proc->mutex, proc);
  proc->last_waiter = NULL;
  DEBUG_LOG(2, "%p signaling availability\n", proc);
  task_condition_signal(proc->cv);
  task_mutex_unlock(proc->mutex, proc);
}

void
proc_wait_for_available(processor_t waitee, processor_t waiter)
{
  task_mutex_lock(waitee->mutex, waiter);
  waitee->last_waiter = waiter;
  DEBUG_LOG(2, "%p waiting availablitity of %p\n", waiter, waitee);
  while (waitee->last_waiter != waiter)
    {
      task_condition_wait(waitee->cv, waitee->mutex, waiter);
    }
  task_mutex_unlock(waitee->mutex, waiter);
}

static
void
destroy_priv_queue(gpointer key, gpointer value, gpointer user_data)
{
  processor_t supplier_proc = (processor_t) key;
  priv_queue_t q = (priv_queue_t) value;
  processor_t client_proc = (processor_t) user_data;

  priv_queue_shutdown(q, client_proc);
}

void
proc_deref_priv_queues(processor_t proc)
{
  g_hash_table_foreach (proc->privq_cache, destroy_priv_queue, proc);
}


static
void
proc_loop(processor_t proc)
{
  DEBUG_LOG(1, "%p starting\n", proc);
  while (proc->ref_count > 0)
    {
      proc_maybe_yield(proc);

      // Dequeue a private queue from the queue of queues.
      priv_queue_t priv_queue;
      bqueue_dequeue_wait(proc->qoq, (void**)&priv_queue, proc);

      if (priv_queue == NULL)
        {
          // This decrement corresponds to shutting down the processor.
          // If the refcount is still above 0 then there may still be
          // outstanding private queues that want to log their work.
          int ref_count = __sync_sub_and_fetch(&proc->ref_count, 1);
          proc_deref_priv_queues(proc);
          assert (ref_count >= 0);
          if (ref_count == 0)
            {
              break;
            }
        }

      closure_t clos = NULL;
      while (priv_queue != NULL)
        {
          // Dequeue a closure to perform
          if (clos == NULL)
            { 
              clos = priv_dequeue(priv_queue, proc);
            }

          // If its empty we set the processor to 'available' and notify
          // any processors performing wait-conditions to wake up and
          // retry their conditions.
          if (clos == NULL)
            {
              notify_available(proc);
              break;
            }

          if (closure_is_end(clos))
            {
              free(clos);
              // The end of a private queue decrements the ref_count again.
              // This should have been incremented initially when the private
              // queue was bound to this processor.
              int n = __sync_sub_and_fetch(&proc->ref_count, 1);
              priv_queue_free(priv_queue);
              notify_available(proc);
              break;
            }

          closure_apply(clos, NULL);

          if (__sync_bool_compare_and_swap(&clos->next, NULL, priv_queue))
            {
              // If we finished the application first, then be sure to
              // fetch another closure from the queue.
              clos = NULL;
            }
          else
            {
              // If there's a valid closure in the next pointer, then
              // make it the next closure to process, and free the 
              // closure.
              closure_t tmp = clos;
              clos = clos->next;
              closure_free(tmp);
            }
        }
    }
}


void
proc_wake(processor_t proc)
{ 
  while(task_get_state(proc->task) != TASK_WAITING);
  task_set_state(proc->task, TASK_RUNNABLE);
  sync_data_enqueue_runnable(proc->task->sync_data, proc);
}

void
proc_yield_to_executor(processor_t proc)
{
  DEBUG_LOG(2, "%p yielding to executor %p\n", proc, proc->executor);
  yield_to(proc->task, proc->executor->task);
}

void
proc_sleep(processor_t proc, struct timespec duration)
{
  sync_data_add_sleeper(proc->task->sync_data, proc, duration);
  yield_to(proc->task, proc->executor->task);
}


processor_t
proc_new_with_func(sync_data_t sync_data, void (*func)(processor_t))
{
  processor_t proc = (processor_t) malloc(sizeof(struct processor));
  proc->qoq = bqueue_new(25000);
  proc->task = task_make(sync_data);
  proc->id = global_id++;

  proc->available = true;
  proc->mutex = task_mutex_new();
  proc->cv = task_condition_new();

  proc->privq_cache = g_hash_table_new(NULL, NULL);

  proc->ref_count = 1;

  reset_stack_to((void (*)(void*))func, proc);

  sync_data_register_proc(sync_data);
  sync_data_enqueue_runnable(sync_data, proc);

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
  return proc_new(other_proc->task->sync_data);
}

processor_t
proc_new_root(sync_data_t sync_data, void (*root)(processor_t))
{
  return proc_new_with_func (sync_data, root);
}


void
proc_shutdown(processor_t proc, processor_t wait_proc)
{
  bqueue_enqueue_wait(proc->qoq, NULL, wait_proc);
}

void
proc_free(processor_t proc)
{
  sync_data_deregister_proc(proc->task->sync_data);

  task_free(proc->task);
  task_condition_free(proc->cv);
  task_mutex_free(proc->mutex);

  bqueue_free(proc->qoq);


  // Since *this* processor won't push any more work into its
  // private queues, we shut them down.

  g_hash_table_destroy(proc->privq_cache);

  free (proc);
}
