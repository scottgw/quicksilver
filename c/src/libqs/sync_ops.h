/*! \file */

#ifndef _SYNC_OPS_H
#define _SYNC_OPS_H

#include <stdint.h>
#include <glib.h>

#include "types.h"

#ifdef __cplusplus
extern "C" {
#endif

struct sleeper
{
  sched_task_t stask;
  struct timespec end_time;
};

typedef struct sleeper* sleeper_t;

/*!
  Constructs a new global state for the given maximum number of
  lightweight tasks

  \param max_tasks maximum number of lightweight tasks that are possible
  \return new global state
*/
sync_data_t
sync_data_new(uint32_t max_tasks);

/*!
  Releases the resources associated with the global state.

  \param sync_data global state to free
*/
void
sync_data_free(sync_data_t sync_data);

/*!
  Create the given number of executors.

  \param sync_data where to store the executors
  \param n the number of executors to create
*/
void
sync_data_create_executors(sync_data_t sync_data, uint32_t n);

/*!
  Barrier that will wait for all executors to start

  \param sync_data global data where the barrier is
*/
void
sync_data_barrier_wait(sync_data_t sync_data);

/*!
  Wait for all the executors in the global data to finish executing.

  \param sync_data global data that holds the executors
*/
void
sync_data_join_executors(sync_data_t sync_data);


/*!
  Get the global list of executors.

  \param sync_data the global state
  \return the list of executors
*/
GArray*
sync_data_executors(sync_data_t sync_data);

/*!
  Signal that new work is available. This will wake listeners
  that are waiting via sync_data_wait_for_work. Note that the
  signaling isn't race-free, so any listeners should also have a timeout
  mechanism (sync_data_wait_for_work does).

  \param sync_data global state
*/
void
sync_data_signal_work(sync_data_t sync_data);

/*!
  Waits for new work to arrive.
  The waiting is done with a timeout, so this routine will always return
  in a bounded amount of time.

  \param sync_data global state
  \return true if the program is still running, false if there are no
  more processors registered in the global state.
*/
bool
sync_data_wait_for_work(sync_data_t sync_data);

/*!
  Register a single processor in the global state.

  \param sync_data global state
*/
void
sync_data_register_proc(sync_data_t sync_data);

/*!
  Deregister a single processor in the global state.

  \param sync_data global state
*/
void
sync_data_deregister_proc(sync_data_t sync_data);

/*!
  Count the number of registered processors.

  \param sync_data global state
  \return the number of registered processors
*/
uint64_t
sync_data_num_processors(sync_data_t sync_data);

/*!
  Puts a runnable processor on the global run queue.

  \param sync_data global state
  \param stask processor to enqueue
*/
void
sync_data_enqueue_runnable(sync_data_t sync_data, sched_task_t stask);

/*!
  Try to dequeue a processor from the global run queue.

  \param sync_data global state
  \param exec the executor requesting the processor
  \param [out] processor that may be dequeued, invalid if the return value is false
  \return true if a processor was dequeue, false otherwise
*/
bool
sync_data_try_dequeue_runnable(sync_data_t sync_data,
                               executor_t exec,
                               sched_task_t *stask);

/*!
  Dequeue a processor from the global run queue, waiting if there are none.

  \param sync_data global state
  \param exec the executor requesting the processor
  \return  processor that is dequeued, NULL if shutdown should occur
*/
sched_task_t
sync_data_dequeue_runnable(sync_data_t sync_data, executor_t exec);

/*!
  Add a sleeping processor to the global state.

  \param sync_data global state
  \param stask processor to sleep
  \param duration duration of the sleep.
*/
void
sync_data_add_sleeper(sync_data_t sync_data,
                      sched_task_t stask,
                      struct timespec duration);

/*!
  Get the list of sleeping processors.

  \param sync_data global state
  \return a queue of all sleeping processors.
*/
queue_impl_t
sync_data_get_sleepers(sync_data_t sync_data);


#ifdef __cplusplus
}
#endif


#endif // _SYNC_OPS_H
