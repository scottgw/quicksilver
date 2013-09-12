#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include "trace.h"
#include "internal/ws_deque.h"

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
void
circ_array_free(circ_array_t c_array)
{
  free(c_array->array);
  free(c_array);
}

static
int64_t
circ_array_size(circ_array_t c_array)
{
  return c_array->size;
}

static
void*
circ_array_get(circ_array_t c_array, int64_t i)
{
  void* data;
  __atomic_load(&c_array->array[i % circ_array_size(c_array)],
                 &data,
                 __ATOMIC_RELAXED);
  return data;
}

static
void
circ_array_put(circ_array_t c_array, int64_t i, void* data)
{
  __atomic_store(&c_array->array[i % circ_array_size(c_array)],
                 &data,
                 __ATOMIC_RELAXED);
}

static
circ_array_t
circ_array_grow(circ_array_t c_array, int64_t bottom, int64_t top)
{
  circ_array_t new_c_array = circ_array_new(c_array->log_size + 1);
  
  for(int64_t i = top; i < bottom; i++)
    {
      circ_array_put(new_c_array, i, circ_array_get(c_array, i));
    }
  return new_c_array;
}


struct ws_deque
{
  size_t top;
  size_t bottom;
  circ_array_t c_array;
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

void
ws_deque_free(ws_deque_t wsd)
{
  circ_array_free (wsd->c_array);
  free(wsd);
}

int64_t
ws_deque_size(ws_deque_t wsd)
{
  return wsd->bottom - wsd->top;
}

void
ws_deque_push_bottom(ws_deque_t wsd, void* data)
{
  assert(data != NULL);

  size_t b;
  size_t t;
  circ_array_t c_array;

  __atomic_load(&wsd->bottom, &b, __ATOMIC_RELAXED);
  __atomic_load(&wsd->top, &t, __ATOMIC_ACQUIRE);
  __atomic_load(&wsd->c_array, &c_array, __ATOMIC_RELAXED);

  if (b - t > circ_array_size(c_array) - 1)
    {
      c_array = circ_array_grow(c_array, b, t);
      __atomic_store(&wsd->c_array, &c_array, __ATOMIC_SEQ_CST);
    }
  circ_array_put(c_array, b, data);

  __atomic_thread_fence(__ATOMIC_RELEASE);
  __atomic_store_8(&wsd->bottom, b + 1, __ATOMIC_RELAXED);
}


bool
ws_deque_pop_bottom(ws_deque_t wsd, void** data)
{
  size_t b;
  size_t t;
  circ_array_t c_array;

  b = __atomic_load_8(&wsd->bottom, __ATOMIC_RELAXED) - 1;
  __atomic_load(&wsd->c_array, &c_array, __ATOMIC_RELAXED);
  __atomic_store(&wsd->bottom, &b, __ATOMIC_RELAXED);
  __atomic_thread_fence(__ATOMIC_SEQ_CST);
  __atomic_load(&wsd->top, &t, __ATOMIC_RELAXED);

  void* temp_data;
  bool ret;

  if (t <= b)
    {
      // Indicates a non empty queue
      temp_data = circ_array_get(c_array, b);
      if (t == b)
        {
          if (!__atomic_compare_exchange_8(&wsd->top, &t, t + 1, false,
                                           __ATOMIC_SEQ_CST,
                                           __ATOMIC_RELAXED))
            {
              // It's already false, but okay this is explicit.
              ret = false;
            }
          else
            {
              assert(temp_data != NULL);
              *data = temp_data;
              ret = true;
            }

          __atomic_store_8(&wsd->bottom, b + 1, __ATOMIC_RELAXED);
        }
      else
        {
          assert(temp_data != NULL);
          *data = temp_data;
          ret = true;
        }
    }
  else
    {
      ret = false; // empty
      __atomic_store_8(&wsd->bottom, b + 1, __ATOMIC_RELAXED);
    }

  return ret;
}


bool
ws_deque_steal(ws_deque_t wsd, void** data)
{
  QS_WS_DEQUE_STEAL_START();
  size_t b;
  size_t t;

  __atomic_load(&wsd->top, &t, __ATOMIC_ACQUIRE);
  __atomic_thread_fence(__ATOMIC_SEQ_CST);
  __atomic_load(&wsd->bottom, &b, __ATOMIC_ACQUIRE);

  bool ret = false;     

  if (t < b)
    {
      circ_array_t c_array;
      __atomic_load(&wsd->c_array, &c_array, __ATOMIC_RELAXED);

      void* temp_data = circ_array_get(c_array, t);

      if (!__atomic_compare_exchange_8(&wsd->top, &t, t + 1, false,
                                       __ATOMIC_SEQ_CST,
                                       __ATOMIC_RELAXED))
        {
          ret = false; // It's already false, but okay this is explicit.
        }
      else
        {
          assert(temp_data != NULL);
          ret = true;
          *data = temp_data;
        }
    }

  if (!ret)
    {
      QS_WS_DEQUE_STEAL_FAIL();
    }

  QS_WS_DEQUE_STEAL_END();
  return ret;
}
