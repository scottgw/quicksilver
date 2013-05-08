#include <stdarg.h>
#include <stdio.h>
#include <unistd.h>

#include "libqs/debug_log.h"

FILE *fd = 0;

void
logs(char* str, ...)
{
  /* if (fd == 0) */
  /*   { */
  /*     fd = fopen("qs.log","w+"); */
  /*   } */

  /* char tmp_str[200]; */

  /* va_list args; */
  /* va_start(args, str); */
  /* vfprintf(stderr, str, args); */
  /* va_end(args); */

  /* write(fileno(fd), tmp_str, n); */

  /* fflush(fd); */
}
