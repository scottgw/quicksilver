#include <assert.h>
#include <fcntl.h>
#include <math.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <unistd.h>


#include <netinet/in.h>

#include <sys/socket.h>
#include <sys/stat.h>

void
qs_init()
{
  //  GC_INIT();
}

void*
qs_malloc(size_t sz)
{
  return malloc(sz);
}


void
exit_with(int64_t i)
{
  exit(i);
}

/*******************************/
/* Array creation and routines */
/*******************************/

void
array_make(void* proc, char*** array, int64_t size)
{
  *array = (char**) malloc(size * sizeof(char*));
}

char*
array_item(void* proc, char*** array, int64_t i)
{
  return (*array)[i];
}

void
array_put(void* proc, char*** array, int64_t i, char* elem)
{
  (*array)[i] = elem;
}

/***********************************/
/* Character and pointer functions */
/***********************************/

int64_t*
new_pointer_64 (int64_t n)
{
  return (int64_t*) malloc(sizeof(int64_t) * n);
}

void
pointer_64_put (int64_t* p, int64_t i, int64_t c)
{
  p[i] = c;
}

int64_t
pointer_64_get (int64_t* p, int64_t i)
{
  return p[i];
}

void
pointer_real_put (int64_t* p, int64_t i, double c)
{
  ((double*)p)[i] = c;
}

double
pointer_real_get (int64_t* p, int64_t i)
{
  return ((double*) p)[i];
}

/************************************
 *  Real math functions
 *
 ***********************************/
double
real_sqrt(double x)
{
  return sqrt(x);
}

double
real_from_int(int64_t i)
{
  return (double) i;
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

int8_t
char_to_int8 (char c)
{
  return c;
}

int64_t
int8_to_int (int8_t i)
{
  return (int64_t) i;
}

int8_t
int_to_int8 (int64_t i)
{
  return (int8_t) i;
}

/*********************
 * Number conversion *
 * *******************/

double
int_to_real(int64_t i)
{
  return (double)i;
}

/**********/
/* String */
/**********/

struct string
{
  int64_t length;
  uint8_t* data;
};

struct string*
real_to_str(double d)
{
  struct string* string = malloc(sizeof(struct string));

  // the defeault (%f) should use 6 decimal places, so 20 characters
  // should be enough for the times we expect (from the benchmarks).
  char* data = malloc(sizeof(char) * 20);
  int len = snprintf(data, 20, "%f", d);
  
  string->data = (uint8_t*) data;
  string->length = len;

  return string;
}

void
print_to_stream(int64_t stream, struct string* str)
{
  write(stream, str->data, str->length);
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


/*************************/
/* Environment variables */
/*************************/

int64_t
get_int_env(struct string *str)
{
  char* cstr = strndup((char*)str->data, str->length);
  char* envstr = getenv(cstr);
  assert (envstr != NULL);
  free(cstr);

  return atoi(envstr);
}


/********/
/* Time */
/********/

double
get_time()
{
  struct timespec now;
  clock_gettime(CLOCK_MONOTONIC, &now);
  return ((double)now.tv_sec) + ((double)now.tv_nsec) / 1000000000.0;
}
