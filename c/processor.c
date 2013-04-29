#include <stdlib.h>
#include <stdint.h>
#include <ucontext.h>
#include <assert.h>

#include "bounded_queue.h"
#include "closure.h"
#include "executor.h"
#include "maybe.h"
#include "notifier.h"
#include "processor.h"
#include "task.h"

int global_id = 0;

void
maybe_yield(processor_t proc, int i)
{
  if (time_is_up == 1)
    {
      time_is_up = 0;
      yield_to(proc->task, proc->executor->task);
    }
}


static
void*
dequeue_wait_maybe(bounded_queue_t q, processor_t proc)
{
  maybe_t maybe;
  bqueue_dequeue_wait(q, (void**)&maybe, proc);
  void *ptr = maybe->load;
  free(maybe);
  return ptr;
}

static
void
enqueue_maybe(bounded_queue_t q, void *ptr)
{
  maybe_t m = (maybe_t) malloc(sizeof(struct maybe));
  m->load = ptr;
  bqueue_enqueue(q, m);
}

closure_t
dequeue_closure(bounded_queue_t q, processor_t proc)
{
  return (closure_t) dequeue_wait_maybe(q, proc);
}

void
enqueue_closure(bounded_queue_t q, closure_t clos)
{
  enqueue_maybe(q, clos);
}

bounded_queue_t
dequeue_private_queue(processor_t proc)
{
  return (bounded_queue_t) dequeue_wait_maybe(proc->qoq, proc);
}

void
enqueue_private_queue(processor_t proc, bounded_queue_t q)
{
  enqueue_maybe(proc->qoq, (void*) q);
}


static
void
notify_available(processor_t proc)
{
  proc->available = true;
  task_condition_signal(proc->cv);
}

void
proc_wait_for_available(processor_t waitee, processor_t waiter)
{
  task_mutex_lock(waitee->mutex, waiter);
  task_condition_wait(waitee->cv, waitee->mutex, waiter);
  task_mutex_unlock(waitee->mutex, waiter);
}

void
proc_loop(void* ptr)
{
  processor_t proc = (processor_t)ptr;
  while (true)
    {
      // Dequeue a private queue from the queue of queues.
      bounded_queue_t priv_queue = dequeue_private_queue(proc);

      if (priv_queue == NULL)
        {
          break;
        }

      while (true)
        {
          // Dequeue a closure to perform
          closure_t clos = dequeue_closure(priv_queue, proc);

          // If its empty we set the processor to 'available' and notify
          // any processors performing wait-conditions to wake up and
          // retry their conditions.
          if (clos == NULL)
            {
              bqueue_free(priv_queue);
              notify_available(proc);
              break;
            }

          closure_apply(clos, NULL);
        }
    }
}

bounded_queue_t
proc_make_private_queue(processor_t proc)
{
  bounded_queue_t q = bqueue_new(1024);
  enqueue_private_queue(proc, q);
  return q;
}

void
proc_wake(processor_t proc)
{
  assert(proc->task->state == TASK_WAITING);
  proc->task->state = TASK_RUNNABLE;
  sync_data_enqueue_runnable(proc->task->sync_data, proc);
}

void
yield_to_executor(processor_t proc)
{
  yield_to(proc->task, proc->executor->task);
}

void
proc_sleep(processor_t proc, struct timespec duration)
{
  sync_data_add_sleeper(proc->task->sync_data, proc, duration);
  yield_to(proc->task, proc->executor->task);
}

processor_t
make_processor(sync_data_t sync_data)
{
  processor_t proc = (processor_t) malloc(sizeof(struct processor));
  proc->qoq = bqueue_new(25000);
  proc->task = task_make(sync_data);
  proc->id = global_id++;

  proc->available = true;
  proc->mutex = task_mutex_new();
  proc->cv = task_condition_new();

  reset_stack_to(proc_loop, proc);

  sync_data_register_proc(sync_data);
  sync_data_enqueue_runnable(sync_data, proc);

  return proc;
}

void
proc_shutdown(processor_t proc)
{
  enqueue_private_queue(proc, NULL);
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

void
reset_stack_to(void (*f)(void*), processor_t proc)
{
  task_set_func(proc->task, f, proc);
}
