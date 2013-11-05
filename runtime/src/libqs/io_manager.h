#ifndef __IO_MANAGER_H
#define __IO_MANAGER_H

#ifdef __cplusplus
extern "C" {
#endif

#include <sys/types.h>
#include "libqs/sched_task.h"


struct io_mgr;
typedef struct io_mgr* io_mgr_t;

/*!
  Construct a new IO manager referencing the global state.
  The global state is used to awaken schedulable tasks, as they cannot
  be placed directly into a work stealing queue.

  \param sync_data global data
  \return new IO manager
*/
io_mgr_t
io_mgr_new(sync_data_t sync_data);

/*!
  Free the memory associated with the IO manager

  \param mgr IO manager to free
*/
void
io_mgr_free(io_mgr_t mgr);

/*!
  Read bytes from a file descriptor, blocking if necessary.

  \param io_mgr IO manager to use
  \param stask schedulable task to block 
  \param fd the file descriptor to read
  \param buf buffer to read the data into
  \param nbyte number of bytes to read
*/
ssize_t
io_mgr_read(io_mgr_t io_mgr,
            sched_task_t stask,
            int fd, void* buf, size_t nbyte);

/*!
  Write bytes to a file descriptor, blocking if necessary.

  \param io_mgr IO manager to use
  \param stask schedulable task to block 
  \param fd the file descriptor to write to
  \param buf buffer containing data to write
  \param nbyte number of bytes to write
*/
ssize_t
io_mgr_write(io_mgr_t io_mgr,
             sched_task_t stask,
             int fd, void* buf, size_t nbyte);


/*!
  Ask the IO manager to register the file descriptor for reading.
  This uses edge-triggered events so this should be done only after
  the fd is returning, for example, EAGAIN.

  \param mgr the IO manager
  \param stask schedulable task associated with this read
  \param fd the file descriptor to watch
  \return 0 if data fd was added successfully,
    another value if an error occurred.
 */
int
io_mgr_add_read_fd(io_mgr_t mgr, sched_task_t stask, int fd);


/*!
  Ask the IO manager to register the file descriptor for writing.
  This uses edge-triggered events so this should be done only after
  the fd is returning, for example, EAGAIN.

  \param mgr the IO manager
  \param stask schedulable task associated with this write
  \param fd the file descriptor to watch
  \return 0 if data fd was added successfully,
    another value if an error occurred.
 */
int
io_mgr_add_write_fd(io_mgr_t mgr, sched_task_t stask, int fd);


/*!
  Put a schedulable task to sleep until a file descriptor is available
  for reading.

  \param mgr the IO manager
  \param stask schedulable task that is waiting to read
  \param fd the file descriptor to watch
  \return 0 if data fd was added successfully,
    another value if an error occurred.
 */
void
io_mgr_wait_read_fd(io_mgr_t mgr, sched_task_t stask, int fd);

/*!
  Put a schedulable task to sleep until a file descriptor is available
  for writing.

  \param mgr the IO manager
  \param stask schedulable task that is waiting to write
  \param fd the file descriptor to watch
  \return 0 if data fd was added successfully,
    another value if an error occurred.
 */
void
io_mgr_wait_write_fd(io_mgr_t mgr, sched_task_t stask, int fd);

/*!
  Spawn a new thread to run the IO manager main loop.

  \param io_mgr IO manager to use
*/
void
io_mgr_spawn(io_mgr_t io_mgr);

/*!
  Wait for the IO manager thread to stop (should happen after issuing
  io_mgr_set_done

  \param io_mgr IO manager thread to wait for
*/
void
io_mgr_join(io_mgr_t io_mgr);

/*!
  Ask an IO manager thread to stop.

  \param io_mgr IO manager to request to stop
*/
void
io_mgr_set_done(io_mgr_t io_mgr);

#ifdef __cplusplus
}
#endif

#endif // __IO_MANAGER_H
