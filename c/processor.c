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
#include "private_queue.h"
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

void*
dequeue_wait_maybe(bounded_queue_t q, processor_t proc)
{
  void* ptr;
  bqueue_dequeue_wait(q, (void**)&ptr, proc);
  return ptr;
}

void
enqueue_maybe(bounded_queue_t q, void *ptr, processor_t proc)
{
  bqueue_enqueue_wait(q, ptr, proc);
}

closure_t
dequeue_closure(bounded_queue_t q, processor_t proc)
{
  return (closure_t) dequeue_wait_maybe(q, proc);
}

void
enqueue_closure(bounded_queue_t q, closure_t clos, processor_t proc)
{
  enqueue_maybe(q, clos, proc);
}

priv_queue_t
dequeue_private_queue(processor_t proc)
{
  return (priv_queue_t) dequeue_wait_maybe(proc->qoq, proc);
}

void
enqueue_private_queue(processor_t proc, priv_queue_t q, processor_t wait_proc)
{
  enqueue_maybe(proc->qoq, (void*) q, wait_proc);
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
      priv_queue_t priv_queue = dequeue_private_queue(proc);

      if (priv_queue == NULL)
        {
          break;
        }

      while (true)
        {
          // Dequeue a closure to perform
          closure_t clos = priv_dequeue(priv_queue, proc);

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
        }
    }
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
proc_shutdown(processor_t proc, processor_t wait_proc)
{
  enqueue_private_queue(proc, NULL, wait_proc);
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
