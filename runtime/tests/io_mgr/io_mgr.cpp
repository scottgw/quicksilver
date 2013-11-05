#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <libqs/io_manager.h>
#include <libqs/sync_ops.h>
#include <libqs/sync_ops.h>
#include <libqs/sched_task.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define SERV_PORT

io_mgr_t io_mgr;

static
void
set_nonblocking(int fd)
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

void
send_func(sched_task_t stask)
{ 
  int client_sfd = socket(AF_INET, SOCK_STREAM, 0);
  if (client_sfd == -1)
    {
      printf("cannot create socket\n");
      exit(1);
    }

  struct sockaddr_in client;
  memset(&client, 0, sizeof(client));
  client.sin_family = AF_INET;
  client.sin_addr.s_addr = inet_addr("127.0.0.1");
  client.sin_port = htons(7777);

  // Loop as we know the other socket will connect eventually, needs at least 2
  // executors though.
  while (connect(client_sfd, (struct sockaddr*)&client, sizeof(client)) != 0);

  set_nonblocking(client_sfd);

  for (int i = 0; i < 20; i++)
    {
      char str[] = "Hello world\n";
      size_t str_nbytes = sizeof(str);
      printf("Client writing to server\n");
      io_mgr_write(io_mgr, stask, client_sfd, str, str_nbytes);
    }
  close(client_sfd);
}

static
void
read_from_client(sched_task_t stask, int client_sfd)
{
  ssize_t client_s;
  char buf[8];

  printf("reading from client\n");

  do
    {
      client_s = io_mgr_read(io_mgr, stask, client_sfd, buf, sizeof(buf));
      write(STDOUT_FILENO, buf, client_s);
    } while (client_s != 0);

  printf("done reading from client\n");
}

void
recv_func(sched_task_t stask)
{
  int serv_sfd = socket(AF_INET, SOCK_STREAM, 0);
  if (serv_sfd == -1)
    {
      printf("cannot create socket\n");
      exit(1);
    }

  struct sockaddr_in server;
  memset(&server, 0, sizeof(server));
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = INADDR_ANY;
  server.sin_port = htons(7777);
  
  int bind_result =
    bind(serv_sfd, (struct sockaddr*)&server, sizeof(struct sockaddr_in));

  if (bind_result != 0)
    {
      printf("cannot bind socket\n");
      close(serv_sfd);
      exit(1);
    }

  if (listen(serv_sfd, 20) != 0)
    {
      printf("cannot listen on socket\n");
      close(serv_sfd);
      exit(1);
    }

  set_nonblocking (serv_sfd);

  struct sockaddr_in client;
  memset(&client, 0, sizeof(client));
  socklen_t client_len = sizeof(struct sockaddr_in);
  
  int client_sfd = accept(serv_sfd, (struct sockaddr*)&client, &client_len);

  if (client_sfd > 0)
    {
      read_from_client(stask, client_sfd);
    }
  else
    {
      switch (errno)
        {
        case EAGAIN:
          printf("Server would block!\n");
          io_mgr_add_read_fd(io_mgr, stask, serv_sfd);
          io_mgr_wait_read_fd(io_mgr, stask, serv_sfd);
          printf("Now has some client\n");
          
          client_sfd = accept(serv_sfd, (struct sockaddr*)&client, &client_len);
          read_from_client(stask, client_sfd);

          break;
        default:
          printf("Some other status %s\n", strerror(errno));
          close(serv_sfd);
          exit(1);
          break;
        }
    }

  close(serv_sfd);
  io_mgr_set_done(io_mgr);
}

int main(int argc, char** argv)
{
  sync_data_t sync_data = sync_data_new(20000);
  sched_task_t send_stask = stask_new(sync_data);
  sched_task_t recv_stask = stask_new(sync_data);
  io_mgr = io_mgr_new(sync_data);

  io_mgr_spawn(io_mgr);

  stask_set_func(send_stask, (void (*)(void*))send_func, send_stask);
  stask_set_func(recv_stask, (void (*)(void*))recv_func, recv_stask);

  sync_data_create_executors (sync_data, 4);

  sync_data_enqueue_runnable(sync_data, send_stask);
  sync_data_enqueue_runnable(sync_data, recv_stask);
  
  sync_data_join_executors(sync_data);
  sync_data_free(sync_data);
  io_mgr_join(io_mgr);

  return 0;
}
