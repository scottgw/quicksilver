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

void
send_func(sched_task_t stask)
{ 
  int client_sfd = socket(AF_INET, SOCK_STREAM, 0);
  io_mgr_set_nonblocking(client_sfd);

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
  int conn_s;
  do
    {
      conn_s = connect(client_sfd, (struct sockaddr*)&client, sizeof(client));
    } while (conn_s == - 1);

  printf("send_func: client_sfd %d conn_s %d\n", client_sfd, conn_s);

  for (int i = 0; i < 20; i++)
    {
      char str[] = "Hello world\n";
      size_t str_nbytes = sizeof(str);
      printf("Client writing to server\n");
      io_mgr_write(io_mgr, stask, client_sfd, str, str_nbytes);
    }

  close(client_sfd);
  printf("send_func: done\n");
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
  printf("recv_func: start\n");
  int serv_sfd = socket(AF_INET, SOCK_STREAM, 0);
  io_mgr_set_nonblocking (serv_sfd);

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

  printf("recv_func: bind\n");  
  int bind_result =
    bind(serv_sfd, (struct sockaddr*)&server, sizeof(struct sockaddr_in));

  if (bind_result != 0)
    {
      printf("cannot bind socket\n");
      close(serv_sfd);
      exit(1);
    }

  printf("recv_func: listen\n");  
  if (listen(serv_sfd, 20) != 0)
    {
      printf("cannot listen on socket\n");
      close(serv_sfd);
      exit(1);
    }

  printf("recv_func: accept\n");
  int client_sfd = io_mgr_accept(io_mgr, stask, serv_sfd);
  printf("recv_func: client_sfd %d\n", client_sfd);

  io_mgr_set_nonblocking(client_sfd);

  printf("recv_func: read_from_client\n");
  read_from_client(stask, client_sfd);

  close(serv_sfd);
  printf("recv_func: done\n");
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
  io_mgr_set_done(io_mgr);
  sync_data_free(sync_data);
  io_mgr_join(io_mgr);

  return 0;
}
