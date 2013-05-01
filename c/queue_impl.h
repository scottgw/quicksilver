#ifdef __cplusplus
extern "C" {
#endif

  struct queue_impl;
  typedef struct queue_impl * queue_impl_t;

  queue_impl_t
  queue_impl_new(size_t size);

  void
  queue_impl_free(queue_impl_t q);

  void
  queue_impl_use(queue_impl_t q);

  bool
  queue_impl_enqueue(queue_impl_t q, void* data);

  bool
  queue_impl_dequeue(queue_impl_t q, void** data_out);

  void
  queue_impl_enqueue_wait(queue_impl_t q, void* data);

  void
  queue_impl_dequeue_wait(queue_impl_t q, void** data_out);

#ifdef __cplusplus
}
#endif
