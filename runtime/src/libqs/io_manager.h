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
  Ask the IO manager to suspend a schedulable task until
  the file descriptor is has data ready to read.

  \param mgr the IO manager
  \param stask the schedulable task to awake when ready
  \param fd the file descriptor to watch
  \return 0 if data is ready to be read, another value if an error occurred.
 */
int
io_wait_fd(io_mgr_t mgr, sched_task_t stask, int fd);
