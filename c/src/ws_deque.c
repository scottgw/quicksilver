#include <stdlib.h>
#include <stdio.h>

#include "libqs/ws_deque.h"

struct circ_array
{
  int64_t log_size;
  uint64_t size;
  uint64_t size_mask;
  void **array;
};

typedef struct circ_array* circ_array_t;

static
circ_array_t
circ_array_new(int64_t log_size)
{
  circ_array_t c_array = (circ_array_t) malloc(sizeof(struct circ_array));

  c_array->log_size = log_size;
  c_array->size = 1 << log_size;
  c_array->size_mask = c_array->size - 1;
  c_array->array = malloc((1 << log_size) * sizeof(void*));
  
  return c_array;
}

static
int64_t
circ_array_size(circ_array_t c_array)
{
  return c_array->size;
}


void*
circ_array_get(circ_array_t c_array, int64_t i)
{
  return c_array->array[i & c_array->size_mask];
}

static
void
circ_array_put(circ_array_t c_array, int64_t i, void* data)
{
  c_array->array[i % circ_array_size(c_array)] = data;
}

static
circ_array_t
circ_array_grow(circ_array_t c_array, int64_t bottom, int64_t top)
{
  circ_array_t new_c_array = circ_array_new(c_array->log_size + 1);
  
  for(int64_t i = bottom; i < top; i++)
    {
      circ_array_put(new_c_array, i, circ_array_get(c_array, i));
    }
  return new_c_array;
}


struct ws_deque
{
  volatile int64_t top;
  volatile int64_t bottom;
  volatile circ_array_t c_array;
};

ws_deque_t
ws_deque_new()
{
  ws_deque_t wsd = (ws_deque_t) malloc(sizeof(struct ws_deque));

  wsd->top = 0;
  wsd->bottom = 0;
  wsd->c_array = circ_array_new(10);

  return wsd;
}

int64_t
ws_deque_size(ws_deque_t wsd)
{
  return wsd->bottom - wsd->top;
}

void
ws_deque_push_bottom(ws_deque_t wsd, void* data)
{
  int64_t b = wsd->bottom;
  int64_t t = wsd->top;
  circ_array_t c_array = wsd->c_array;

  int64_t size = b - t;

  if (size >= circ_array_size(c_array) - 1)
    {
      c_array = circ_array_grow(c_array, b, t);
      wsd->c_array = c_array;
    }

  circ_array_put(c_array, b, data);
  /* printf("%p <<<\n", data); */
  wsd->bottom = b + 1;
}


bool
ws_deque_pop_bottom(ws_deque_t wsd, void** data)
{
  int64_t b = wsd->bottom;
  circ_array_t c_array = wsd->c_array;

  b = b - 1;
  wsd->bottom = b;

  int64_t t = wsd->top;

  int64_t size = b - t;

  if (size < 0)
    {
      wsd->bottom = t;
      return false;
    }

  *data = circ_array_get(c_array, b);

  if (size > 0)
    {
      /* printf("<<< %p\n", *data); */
      return true;
    }

  bool result = true;

  if (!__sync_bool_compare_and_swap(&wsd->top, t, t + 1))
    {  
      result = false;
    }

  if (result)
    {
      /* printf("<<< %p\n", *data); */
    }
  wsd->bottom = t + 1;
  return result;
}


bool
ws_deque_steal(ws_deque_t wsd, void** data)
{
  int64_t t = wsd->top;
  int64_t b = wsd->bottom;
  circ_array_t c_array = wsd->c_array;

  int64_t size = b - t;

  if (size <= 0)
    {
      return false;
    }

  *data = circ_array_get(c_array, t);

  if (!__sync_bool_compare_and_swap(&wsd->top, t, t + 1))
    {
      return false;
    }

  /* printf("<<< %p (stolen)\n", *data); */
  return true;
}
