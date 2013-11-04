#include "libqs/io_manager.h"
#include "libqs/sync_ops.h"
#include "internal/task.h"
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/epoll.h>
#include <unistd.h>

#define MAX_EVENTS 1
#define EPOLL_TIMEOUT 50

struct io_mgr
{
  bool alive;
  int epoll_fd;
  sync_data_t sync_data;
};

io_mgr_t
io_mgr_new(sync_data_t sync_data)
{
  io_mgr_t io_mgr = (io_mgr_t)malloc(sizeof(struct io_mgr));
  int fd;

  fd = epoll_create1(EPOLL_CLOEXEC);

  io_mgr->sync_data = sync_data;
  io_mgr->epoll_fd = fd;
  io_mgr->alive = true;

  return io_mgr;
}

void
io_mgr_free(io_mgr_t io_mgr)
{
  close(io_mgr->epoll_fd);
  free(io_mgr);
}

void
io_mgr_loop(io_mgr_t io_mgr)
{
  struct epoll_event events[MAX_EVENTS];
  
  while (io_mgr->alive)
    {
      int n = epoll_wait (io_mgr->epoll_fd, events, MAX_EVENTS, EPOLL_TIMEOUT);
      for (int i = 0; i < n; i++)
        {
          struct epoll_event ev = events[i];
          sched_task_t stask = ev.data.ptr;

          // Resume the stask, waiting for it to really become waiting.
          while (task_get_state(stask->task) != TASK_WAITING);
          task_set_state(stask->task, TASK_RUNNABLE);
          sync_data_enqueue_runnable (io_mgr->sync_data, stask);
        }
      if (n < 0)
        {
          fprintf(stderr, "io_mgr_loop, error in epoll_wait %d\n", errno);
        }
    }
}

static
int
io_add_fd(io_mgr_t mgr, sched_task_t stask, int flags, int fd)
{
  struct epoll_event ev;
  int code;

  ev.events = flags;
  ev.data.ptr = stask;

  code = epoll_ctl(mgr->epoll_fd, EPOLL_CTL_ADD, fd, &ev);

  if (code == 0)
    {
      // Everything goes well nothing to do.
    }
  else
    {
      // FIXME: patently terrible error handling.
      switch (errno)
        {
        case EEXIST:
          code = epoll_ctl(mgr->epoll_fd, EPOLL_CTL_MOD, fd, &ev);
          if (code == 0)
            {
              break;
            }
        default:
          fprintf(stderr, "io_add_fd: error %d\n", errno);
          break;
        }
    }

  return code;
}

int
io_add_read_fd(io_mgr_t mgr, sched_task_t stask, int fd)
{
  return io_add_fd(mgr, stask, EPOLLIN | EPOLLET, fd);
}


int
io_add_write_fd(io_mgr_t mgr, sched_task_t stask, int fd)
{
  return io_add_fd(mgr, stask, EPOLLOUT | EPOLLET, fd);
}

static
void
io_wait_fd(io_mgr_t mgr, sched_task_t stask, int fd)
{
  task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);
  stask_yield_to_executor(stask);
}

void
io_wait_read_fd(io_mgr_t mgr, sched_task_t stask, int fd)
{
  io_wait_fd(mgr, stask, fd);
}


void
io_wait_write_fd(io_mgr_t mgr, sched_task_t stask, int fd)
{
  io_wait_fd(mgr, stask, fd);
}
