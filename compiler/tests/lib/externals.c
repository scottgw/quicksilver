#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <sys/socket.h>
#include <netinet/in.h>

char*
new_pointer_8 (uint32_t n)
{
  return (char*) malloc(sizeof(char) * n);
}

void
pointer_8_put (char* p, int i, int8_t v)
{
  p[i] = v;
}

struct string
{
  int64_t length;
  char* data;
};

void
print(struct string* str)
{
  write(STDOUT_FILENO, str->data, str->length);
}

int
new_tcp_socket()
{
  return socket(AF_INET, SOCK_STREAM, 0);
}

int
socket_bind(int socketfd, int port)
{
  struct sockaddr_in local_addr;
  
  local_addr.sin_family = AF_INET;
  local_addr.sin_addr.s_addr = INADDR_ANY;
  local_addr.sin_port = htons(8888);

  return bind (socketfd, (struct sockaddr*) &local_addr, sizeof(local_addr));
}

int
socket_listen(int socketfd, int backlog_size)
{
  return listen(socketfd, backlog_size);
}

int
socket_accept(int socketfd)
{
  return accept(socketfd, NULL, NULL);
}

int
socket_recv(int socketfd, void* buf, int len)
{
  return recv(socketfd, buf, len, 0);
}

int
socket_send(int socketfd, void* buf, int len)
{
  return send(socketfd, buf, len, 0);
}
