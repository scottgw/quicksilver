#include "internal/task.h"
#include "libqs/io_manager.h"
#include "libqs/sync_ops.h"
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
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
      if (n < 0)
        {
          fprintf(stderr, "io_mgr_loop, error in epoll_wait %d\n", errno);
          exit(1);
        }

      for (int i = 0; i < n; i++)
        {
          struct epoll_event ev = events[i];
          sched_task_t stask = ev.data.ptr;

          // The stask only ever spends a small bound amount of time
          // transitioning out of IO_UNKNOWN so this should be ok.
          while (stask->io_status == IO_STATUS_UNKNOWN);

          if (stask->io_status == IO_SATISFIED)
            {
              // stask already picked up this event, so we can skip resuming it.
              continue;
            }
          else
            {
              // Since the stask is IO_UNSATISFIED it needs to be resumed.
              // Resume the stask, waiting for it to really become waiting.
              while (task_get_state(stask->task) != TASK_WAITING);
              task_set_state(stask->task, TASK_RUNNABLE);
              sync_data_enqueue_runnable (io_mgr->sync_data, stask);
            }
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

  ev.events = flags | EPOLLONESHOT;
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
          exit(1);
          break;
        }
    }

  stask->io_status = IO_STATUS_UNKNOWN;

  return code;
}

static
void
io_mgr_del_fd(io_mgr_t mgr, int fd)
{
  if (epoll_ctl(mgr->epoll_fd, EPOLL_CTL_DEL, fd, NULL) != 0)
    {
      switch (errno)
        {
        case ENOENT:
          break;
        default:
          fprintf(stderr, "io_mgr_del_fd: error %s\n", strerror(errno));
          exit(1);
        }
    }
}

static
void
io_mgr_wait_fd(io_mgr_t mgr, sched_task_t stask, int fd)
{
  stask->io_status = IO_UNSATISFIED;
  task_set_state(stask->task, TASK_TRANSITION_TO_WAITING);
  stask_yield_to_executor(stask);
}

#define IO_PROTOCOL(op_str, io_op, epoll_op, cond)                      \
  {                                                                     \
    io_op;                                                              \
    if (cond)                                                           \
      {                                                                 \
        switch (errno)                                                  \
          {                                                             \
          case EAGAIN:                                                  \
            io_mgr_add_fd(io_mgr, stask, epoll_op, fd);                 \
            io_op;                                                      \
            if (cond)                                                   \
              {                                                         \
                switch (errno)                                          \
                  {                                                     \
                  case EAGAIN:                                          \
                    io_mgr_wait_fd(io_mgr, stask, fd);                  \
                    io_op;                                              \
                    break;                                              \
                  default:                                              \
                    fprintf(stderr, "io_protocol: %s\n", strerror(errno)); \
                    exit(1);                                            \
                    break;                                              \
                  }                                                     \
              }                                                         \
            break;                                                      \
          default:                                                      \
            fprintf(stderr, "io_protocol: %s\n", strerror(errno));      \
            exit(1);                                                    \
            break;                                                      \
          }                                                             \
      }                                                                 \
    stask->io_status = IO_SATISFIED;                                    \
    io_mgr_del_fd (io_mgr, fd);                                         \
  }                                                                     \
  
ssize_t
io_mgr_read(io_mgr_t io_mgr,
             sched_task_t stask,
             int fd, void* buf, size_t nbyte)
{
  ssize_t read_bytes;

  IO_PROTOCOL("read1", read_bytes = read (fd, buf, nbyte), EPOLLIN, read_bytes == -1);

  return read_bytes;
}

ssize_t
io_mgr_write(io_mgr_t io_mgr,
             sched_task_t stask,
             int fd, void* buf, size_t nbyte)
{
  ssize_t written_bytes;

  IO_PROTOCOL("write1", written_bytes = write (fd, buf, nbyte), EPOLLIN, written_bytes == -1);

  return written_bytes;
}

int
io_mgr_accept(io_mgr_t io_mgr, sched_task_t stask, int fd)
{
  struct sockaddr_in client;
  memset(&client, 0, sizeof(client));
  socklen_t client_len = sizeof(struct sockaddr_in);

  int client_sfd;

  IO_PROTOCOL("accept",
              client_sfd = accept(fd, (struct sockaddr*)&client, &client_len),
              EPOLLIN,
              client_sfd < 0);

  return client_sfd;
}

void
io_mgr_set_nonblocking(int fd)
{
  int flags, s;

  flags = fcntl(fd, F_GETFL, 0);
  if (flags == -1)
    {
      printf("cannot get flags\n");
      exit(1);
    }

  s = fcntl(fd, F_SETFL, flags | O_NONBLOCK);
  if (s == -1)
    {
      printf("cannot set flags\n");
      exit(1);
    }
}

