#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

char*
new_pointer_8 (uint32_t n)
{
  return (char*) malloc(sizeof(char) * n);
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
