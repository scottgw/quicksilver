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
      maybe_t maybe_q;
      bqueue_dequeue_wait(proc->qoq, (void**)&maybe_q, proc);

      // If its the end-signal, just break out of this loop and do the cleanup.
      if (maybe_q->load == NULL)
        break;

      bounded_queue_t local_queue = (bounded_queue_t) maybe_q->load;
      free(maybe_q);
      while (true)
        {
          // Dequeue a closure to perform
          maybe_t maybe_clos;
          bqueue_dequeue_wait(local_queue, (void**)&maybe_clos, proc);

          closure_t clos = maybe_clos->load;
          free(maybe_clos);


          // If its empty we set the processor to 'available' and notify
          // any processors performing wait-conditions to wake up and
          // retry their conditions.
          if (clos == NULL)
            {
              notify_available(proc);
              break;
            }

          closure_apply(clos);
          free(clos);
        }
    }
}

bounded_queue_t
proc_make_private_queue(processor_t proc)
{
  bounded_queue_t q = bqueue_new(1024);
  maybe_t m = (maybe_t) malloc(sizeof(struct maybe));
  m->load = q;
  bqueue_enqueue(proc->qoq, m);
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

  proc->qoq = bqueue_new(1024);
  proc->task = task_make(sync_data);
  proc->id = global_id++;

  reset_stack_to(proc_loop, proc);

  sync_data_register_proc(sync_data);
  sync_data_enqueue_runnable(sync_data, proc);

  return proc;
}

void
free_processor(processor_t proc)
{
  task_free (proc->task);
  free (proc);
}

void
reset_stack_to(void (*f)(void*), processor_t proc)
{
  task_set_func(proc->task, f, proc);
}
