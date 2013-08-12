#include <stdlib.h>
#include <stdint.h>
#include <ucontext.h>
#include <assert.h>

#include "libqs/mpsc_blocking.h"
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

/*!
  Look at the previous processor and runs proc_step_state on it
  if it is not the argument processor.

  \param proc processor whose executor will be checked for the
  previous processor.
*/
void
proc_step_previous(processor_t proc)
{
  executor_t exec = proc->executor;
  exec_step_previous (exec, proc);
}

void
proc_step_state(processor_t proc, executor_t exec)
{
  assert(proc->task->state >= TASK_TRANSITION_TO_WAITING);

  switch (proc->task->state)
    {
    case TASK_TRANSITION_TO_RUNNABLE:
      proc->task->state = TASK_RUNNABLE;
      exec_push(exec, proc);
      break;
    case TASK_TRANSITION_TO_WAITING:
      /* printf("%p set to waiting\n", proc); */
      proc->task->state = TASK_WAITING;
      break;
    case TASK_TRANSITION_TO_FINISHED:
      proc->task->state = TASK_FINISHED;
      proc_free(proc);
      break;
    default:
      break;
    }
}


void
proc_yield_to_proc(processor_t from, processor_t to)
{
  assert (from != to);
  executor_t exec = from->executor;
  exec->current_proc = from;

  if (to == NULL)
    {
      yield_to(from->task, exec->task);
    }
  else
    {
      to->executor = exec;

      /* // If this task is to finish, it should come back to this processor. */
      /* // This decision may be dubious, perhaps it should switch the the */
      /* // executor proper? */
      /* if (proc->task->state == TASK_TRANSITION_TO_RUNNABLE) */
      /*   { */
      /*     next_proc->task->next = proc->task; */
      /*   } */
      /* else */
      /*   { */
      /*     next_proc->task->next = exec->task; */
      /*   } */

      to->task->next = exec->task;

      /* printf("%p directly yielding to %p\n", from, to); */
      // Switch to the task we found.
      yield_to(from->task, to->task);
    }

  proc_step_previous(from);
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
  task_condition_signal(proc->cv, proc);
  task_mutex_unlock(proc->mutex, proc);
}

void
proc_wait_for_available(processor_t waitee, processor_t waiter)
{
  task_mutex_lock(waitee->mutex, waiter);
  waitee->last_waiter = waiter;
  DEBUG_LOG(2, "%p waiting availablitity of %p\n", waiter, waitee);
  while (waitee->last_waiter == waiter)
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
proc_duty_loop(processor_t proc, priv_queue_t priv_queue)
{
  while (!priv_queue->shutdown)
    {
      closure_t clos = priv_dequeue(priv_queue, proc);

      // If its empty we set the processor to 'available' and notify
      // any processors performing wait-conditions to wake up and
      // retry their conditions.
      if (clos == NULL)
	{
	  notify_available(proc);
	  break;
	}
      else if (closure_is_end(clos))
	{
	  free(clos);
	  // The end of a private queue decrements the ref_count again.
	  // This should have been incremented initially when the private
	  // queue was bound to this processor.
	  int n = __sync_sub_and_fetch(&proc->ref_count, 1);
	  priv_queue_free(priv_queue);

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
	      notify_available(proc);
	    }

	  // We set the processing wait to false so the next processor
	  // starts out with the default.
	  proc->processing_wait = false;
	  break;
	}
      else if (closure_is_sync(clos))
	{
	  // At the start of wait condition execution the
	  // client is responsible for sending a closure with
	  // the wait-condition flag enabled.
	  //
	  // The client is also responsible for setting it back,
	  // which if done right after the wait-condition holds then
	  // he can do directly on this processor.
	  if (closure_is_wait_sync(clos))
	    {
	      proc->processing_wait = true;
	    }

	  processor_t client = (processor_t) clos->fn;
	  while (client->task->state != TASK_WAITING);
	  proc->task->state = TASK_TRANSITION_TO_WAITING;

	  proc_yield_to_proc(proc, client);
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
  proc_step_previous(proc);
  DEBUG_LOG(1, "%p starting\n", proc);
  while (proc->ref_count > 0)
    {
      proc_maybe_yield(proc);

      // Dequeue a private queue from the queue of queues.
      priv_queue_t priv_queue;
      qo_q_dequeue_wait(proc->qoq, (void**)&priv_queue, proc);

      closure_t clos = NULL;

      proc_duty_loop(proc, priv_queue);


      if (priv_queue->shutdown)
        {
          // This decrement corresponds to shutting down the processor.
          // If the refcount is still above 0 then there may still be
          // outstanding private queues that want to log their work.
          int ref_count = __sync_sub_and_fetch(&proc->ref_count, 1);
          priv_queue_free(priv_queue);
          proc_deref_priv_queues(proc);
          assert (ref_count >= 0);
          if (ref_count == 0)
            {
              break;
            }
        }

    }
}


void
proc_wake(processor_t proc, executor_t exec)
{ 
  while(task_get_state(proc->task) != TASK_WAITING);
  task_set_state(proc->task, TASK_RUNNABLE);
  exec_push(exec, proc);
  /* sync_data_enqueue_runnable(proc->task->sync_data, proc); */
}

void
proc_yield_to_executor(processor_t proc)
{
  assert (proc->task->state >= TASK_TRANSITION_TO_WAITING);
  executor_t exec = proc->executor;
  processor_t next_proc;

  if (sync_data_try_dequeue_runnable (proc->task->sync_data, exec, &next_proc))
    {
      goto yield;
    }

  if (exec_steal(exec, &next_proc))
    {
      goto yield;
      /* printf("%p stole %p\n", proc, next_proc); */
    }

  next_proc = exec_get_work(exec, 8);

 yield:
  proc_yield_to_proc(proc, next_proc);
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

  proc->qoq = qo_q_new(2048);
  proc->task = task_make(sync_data);
  proc->id = global_id++;

  proc->available = true;
  proc->mutex = task_mutex_new();
  proc->cv = task_condition_new();

  proc->processing_wait = false;

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
  priv_queue_t shutdown_q = priv_queue_new(wait_proc);
  shutdown_q->shutdown = true;

  qo_q_enqueue_wait(proc->qoq, shutdown_q, wait_proc);
}

void
proc_free(processor_t proc)
{
  sync_data_deregister_proc(proc->task->sync_data);

  task_free(proc->task);
  task_condition_free(proc->cv);
  task_mutex_free(proc->mutex);

  qo_q_free(proc->qoq);


  // Since *this* processor won't push any more work into its
  // private queues, we shut them down.

  g_hash_table_destroy(proc->privq_cache);

  free (proc);
}
