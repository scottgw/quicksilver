#include <stdbool.h>
#include "../libqs/types.h"

#ifdef __cplusplus
extern "C" {
#endif
  queue_impl_t
  queue_impl_new(size_t size);

  void
  queue_impl_free(queue_impl_t q);

  void
  queue_impl_use(queue_impl_t q);

  int
  queue_impl_size(queue_impl_t q);

  bool
  queue_impl_enqueue(queue_impl_t q, void* data);

  bool
  queue_impl_dequeue(queue_impl_t q, void** data_out);

  void
  queue_impl_enqueue_wait(queue_impl_t q, void* data);

  void
  queue_impl_dequeue_wait(queue_impl_t q, void** data_out);

  queue_impl_t
  queue_impl_filter_out(queue_impl_t q, bool (*pred)(void*, void*), void *user);

#ifdef __cplusplus
}
#endif
