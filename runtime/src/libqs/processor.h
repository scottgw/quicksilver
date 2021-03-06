/*! \file processor.h
  \brief SCOOP processor operations.

  The operations on a SCOOP processor generated code to allocate
  and deallocate a processor, log calls
  (directly through the \a qoq field),
  and wait for events.
  
 */

/*! A SCOOP processor.
 *
 * A processor is responsible for both receiving work items in its queue,
 * and dispatching new work items to other processors.
 */

#ifndef __PROCESSOR_H_
#define __PROCESSOR_H_

#include <stdio.h>
#include <stdbool.h>
#include <glib.h>
#include <pthread.h>

#include "types.h"
#include "sched_task.h"

#ifdef __cplusplus
extern "C" {
#endif

struct processor
{
  /*! Underlying task */
  struct sched_task stask;

  /*! Processor availability flag */
  bool available;

  /*! The previous processor to wait on this one to become available */
  volatile processor_t last_waiter;

  /*! This processor is processing a wait condition */
  bool processing_wait;
  
  /*! This processor's mutex */
  task_mutex_t mutex;

  /*! This processor's condition variable. Used to signal availability */
  task_condition_t cv;

  /*! Cache of private queues for suppliers that this processor has accessed. */
  GHashTable *privq_cache;


  /*! Reservation list */
  GPtrArray* reservation_list;

  /*! Spin lock to use when doing multi-reservations */
  pthread_spinlock_t spinlock;

  #ifdef DISABLE_QOQ
  /*! Lock for the main queue/ */
  task_mutex_t qoq_mutex;
  spsc_queue_t qoq;
  #else
  /*! Queue of queues which the processor will take requests from. */
  qoq_t qoq;
  #endif
};

/*!
  Create a new processor with the sync_data.
  This processor will run a default main loop.

  \param sync_data global synchronization data
  \return new processor
 */
processor_t
proc_new(sync_data_t sync_data);

/*!
  Create a new processor with the global data from other_proc.
  This processor will run a default main loop.

  \param other_proc an existing processor to use the sync_data from.
  \return new processor
*/
processor_t
proc_new_from_other(processor_t other_proc);

/*!
  Create a new processor with the given global data.
  This processor will run the given procedure instead of the default
  main loop.

  \param sync_data global synchronization data
  \param root procedure to run with.
  \return new processor
*/
processor_t
proc_new_root(sync_data_t sync_data, void (*root)(processor_t));

#ifdef DISABLE_QOQ
/*!
  Lock this processor on behalf of the client processor.

  \param !proc this processor
  \param !proc the client processor
*/
void
proc_lock(processor_t proc, processor_t client);
#endif

/*!
  Request a the private queue for the given supplier.
  If one does not exist already, it will create one and return it.
  Otherwise it will return the existing private queue for the supplier.

  \param proc the client processor
  \param supplier_proc the supplier processor
  \return the private queue connecting proc to supplier_proc.
*/
priv_queue_t
proc_get_queue(processor_t proc, processor_t supplier_proc);

/*!
  Wait for a signal that the processor is available (has recently
  completed a work item that may have changed its state).

  \param waitee the processor that we want to listen for events on
  \param waiter the processor that is waiting for something new to happen
*/
void
proc_wait_for_available(processor_t waitee, processor_t waiter);

/*!
  Free the memory for the given processor.

  \param proc processor to free
*/
void
proc_free(processor_t proc);

/*!
  Put the processor to sleep.

  \param proc processor to put to sleep
  \param duration of sleep.
*/
void
proc_sleep(processor_t proc, struct timespec duration);

/*!
  Command this processor to shutdown.
  Since this operates on the queue of queues, this operation may block.
  If so, it will block the requesting processor.

  \param proc processor to shut down
  \param requester processor that wishes proc to shutdown.
*/
void
proc_shutdown(processor_t proc, processor_t requester);

/*!
  This will yield control back to the executor if the time slice is done.
  If the time slice isn't up yet then it will keep running.

  \param proc processor to yield
*/
void
proc_maybe_yield(processor_t proc);

/*!
  Start handler reservation process.
  
  \param client client processor
*/
void
proc_start_reservation (processor_t client);


/*!
  Add handler to reservation.
  
  \param client client processor
  \param supplier supplier processor
*/
void
proc_reserve_handler (processor_t client, processor_t supplier);


/*!
  Finish handler reservation process.
  
  \param client client processor
*/
void
proc_finish_reservation (processor_t client);




#ifdef __cplusplus
}
#endif
#endif // __PROCESSOR_H_
