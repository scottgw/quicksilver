#include "libqs/io_manager.h"
#include "libqs/sync_ops.h"
#include "internal/task.h"
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <unistd.h>

#define MAX_EVENTS 1
#define EPOLL_TIMEOUT 50

struct io_mgr
{
  volatile bool alive;
  int epoll_fd;
  sync_data_t sync_data;
  pthread_t thread;
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
io_mgr_set_done(io_mgr_t io_mgr)
{
  io_mgr->alive = false;
}

static
void*
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
  return NULL;
}

void
io_mgr_spawn(io_mgr_t io_mgr)
{
  pthread_create(&io_mgr->thread, NULL, (void* (*)(void*))io_mgr_loop, io_mgr);
}

void
io_mgr_join(io_mgr_t io_mgr)
{
  pthread_join(io_mgr->thread, NULL);
}

static
int
io_mgr_add_fd(io_mgr_t mgr, sched_task_t stask, int flags, int fd)
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
          fprintf(stderr, "io_mgr_add_fd: error %s\n", strerror(errno));
          break;
        }
    }

  return code;
}

static
void
io_mgr_wait_fd(io_mgr_t mgr, sched_task_t stask, int fd)
{
  task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);
  stask_yield_to_executor(stask);
}

void
io_mgr_wait_read_fd(io_mgr_t mgr, sched_task_t stask, int fd)
{
  io_mgr_wait_fd(mgr, stask, fd);
}

void
io_mgr_wait_write_fd(io_mgr_t mgr, sched_task_t stask, int fd)
{
  io_mgr_wait_fd(mgr, stask, fd);
}

int
io_mgr_add_read_fd(io_mgr_t mgr, sched_task_t stask, int fd)
{
  return io_mgr_add_fd(mgr, stask, EPOLLIN | EPOLLET, fd);
}

int
io_mgr_add_write_fd(io_mgr_t mgr, sched_task_t stask, int fd)
{
  return io_mgr_add_fd(mgr, stask, EPOLLOUT | EPOLLET, fd);
}


// Perform one non EAGAIN read.
static
ssize_t
io_mgr_read1(io_mgr_t io_mgr,
             sched_task_t stask,
             int fd, void* buf, size_t nbyte)
{
  ssize_t read_bytes = read (fd, buf, nbyte);

  if (read_bytes == -1)
    {
      switch (errno)
        {
        case EAGAIN:
          printf("Read would block!\n");
          io_mgr_add_read_fd(io_mgr, stask, fd);
          read_bytes = read (fd, buf, nbyte);
          io_mgr_wait_read_fd(io_mgr, stask, fd);

          if (read_bytes == -1)
            {
              // We didn't read anything before we waited so lets read now
              read_bytes = read (fd, buf, nbyte);
            }

          break;
        default:
          printf("io_mgr_read: problem with read\n");
          exit(1);
          break;
        }
    }
  return read_bytes;
}

ssize_t
io_mgr_read(io_mgr_t io_mgr,
            sched_task_t stask,
            int fd, void* buf, size_t nbyte)
{
  ssize_t read_bytes = 0;
  bool eof = false;

  do
    {
      ssize_t last_read_bytes = io_mgr_read1(io_mgr, stask, fd, buf, nbyte);
      if (last_read_bytes == 0)
        {
          eof = true;
        }
      read_bytes += last_read_bytes;
      buf = (char*)buf + last_read_bytes;
    } while (read_bytes < nbyte && !eof);

  return read_bytes;
}


// Perform one non EAGAIN write.
static
ssize_t
io_mgr_write1(io_mgr_t io_mgr,
              sched_task_t stask,
              int fd, void* buf, size_t nbyte)
{
  ssize_t written_bytes = write (fd, buf, nbyte);

  if (written_bytes == -1)
    {
      switch (errno)
        {
        case EAGAIN:
          printf("Read would block!\n");
          io_mgr_add_write_fd(io_mgr, stask, fd);
          written_bytes = write (fd, buf, nbyte);
          io_mgr_wait_write_fd(io_mgr, stask, fd);

          if (written_bytes == -1)
            {
              // We didn't read anything before we waited so lets read now
              written_bytes = write (fd, buf, nbyte);
            }

          break;
        default:
          printf("io_mgr_read: problem with read\n");
          exit(1);
          break;
        }
    }
  return written_bytes;
}

ssize_t
io_mgr_write(io_mgr_t io_mgr,
             sched_task_t stask,
             int fd, void* buf, size_t nbyte)
{
  ssize_t written_bytes = 0;
  bool eof = false;

  do
    {
      ssize_t last_written_bytes = io_mgr_write1(io_mgr, stask, fd, buf, nbyte);
      if (last_written_bytes == 0)
        {
          eof = true;
        }
      written_bytes += last_written_bytes;
      buf = (char*)buf + last_written_bytes;
    } while (written_bytes < nbyte && !eof);

  return written_bytes;
}
