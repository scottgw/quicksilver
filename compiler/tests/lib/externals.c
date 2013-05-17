#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

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
  int length;
  char* data;
};

void
print(struct string* str)
{
  write(STDOUT_FILENO, str->data, str->length);
}
