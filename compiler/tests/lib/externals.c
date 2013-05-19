#include <fcntl.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

#include <netinet/in.h>

#include <sys/socket.h>
#include <sys/stat.h>

void
exit_with(int64_t i)
{
  exit(i);
}

/***********************************/
/* Character and pointer functions */
/***********************************/

char*
new_pointer_8 (int64_t n)
{
  return (char*) malloc(sizeof(char) * n);
}

void
pointer_8_put (char* p, int64_t i, char c)
{
  p[i] = c;
}

char
pointer_8_get (char* p, int64_t i)
{
  return p[i];
}

char
int8_to_char (int8_t i)
{
  return i;
}


struct string
{
  int64_t length;
  uint8_t* data;
};

void
print(struct string* str)
{
  write(STDOUT_FILENO, str->data, str->length);
}


/*****************************/
/* File descriptor functions */
/*****************************/

int
fd_close(int fd)
{
  return close(fd);
}

int
open_read(struct string* str)
{
  return open((char*)str->data, O_RDONLY);
}

ssize_t
fd_read(int fd, char *buf, uint64_t size)
{
  return read(fd, buf, size);
}

/********************/
/* Socket functions */
/********************/

int
new_tcp_socket()
{
  int socketfd = socket(AF_INET, SOCK_STREAM, 0);
  int enable = 1;

  setsockopt(socketfd, SOL_SOCKET, SO_REUSEADDR, &enable, sizeof(enable));

  return socketfd;
}


int
socket_bind(int socketfd, int port)
{
  struct sockaddr_in local_addr;
  
  local_addr.sin_family = AF_INET;
  local_addr.sin_addr.s_addr = INADDR_ANY;
  local_addr.sin_port = htons(port);

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
socket_recv(int socketfd, struct string* str)
{
  ssize_t recv_size = recv(socketfd, str->data, str->length, 0);
  str->length = recv_size;
  return recv_size;
}

int
socket_send(int socketfd, void* buf, int len)
{
  return send(socketfd, buf, len, 0);
}
