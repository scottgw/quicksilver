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
  logs(2, "%p signaling availability\n", proc);
  task_condition_signal(proc->cv);
  task_mutex_unlock(proc->mutex, proc);
}

void
proc_wait_for_available(processor_t waitee, processor_t waiter)
{
  task_mutex_lock(waitee->mutex, waiter);
  waitee->last_waiter = waiter;
  logs(2, "%p waiting availablitity of %p\n", waiter, waitee);
  while (waitee->last_waiter != waiter)
    {
      task_condition_wait(waitee->cv, waitee->mutex, waiter);
    }
  task_mutex_unlock(waitee->mutex, waiter);
}

static
void
proc_loop(processor_t proc)
{
  logs(1, "%p starting\n", proc);
  while (true)
    {
      proc_maybe_yield(proc);

      // Dequeue a private queue from the queue of queues.
      priv_queue_t priv_queue;
      bqueue_dequeue_wait(proc->qoq, (void**)&priv_queue, proc);

      if (priv_queue == NULL)
        {
          break;
        }

      closure_t clos = NULL;
      while (true)
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
  logs(2, "%p yielding to executor %p\n", proc, proc->executor);
  yield_to(proc->task, proc->executor->task);
}

void
proc_sleep(processor_t proc, struct timespec duration)
{
  sync_data_add_sleeper(proc->task->sync_data, proc, duration);
  yield_to(proc->task, proc->executor->task);
}

processor_t
proc_new(sync_data_t sync_data)
{
  return proc_new_root (sync_data, proc_loop);
}

processor_t
proc_new_from_other(processor_t other_proc)
{
  return proc_new(other_proc->task->sync_data);
}

processor_t
proc_new_root(sync_data_t sync_data, void (*root)(processor_t))
{
  processor_t proc = (processor_t) malloc(sizeof(struct processor));
  proc->qoq = bqueue_new(25000);
  proc->task = task_make(sync_data);
  proc->id = global_id++;

  proc->available = true;
  proc->mutex = task_mutex_new();
  proc->cv = task_condition_new();

  proc->privq_cache = g_hash_table_new(NULL, NULL);

  reset_stack_to((void (*)(void*)) root, proc);

  sync_data_register_proc(sync_data);
  sync_data_enqueue_runnable(sync_data, proc);

  return proc;
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

  free (proc);
}
