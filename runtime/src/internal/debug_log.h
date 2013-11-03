#ifndef _DEBUG_LOG_H
#define _DEBUG_LOG_H

#include <time.h>

typedef enum {
  SYNCOPS_DEQUEUE_START,
  SYNCOPS_DEQUEUE_END,
  PRIVQ_ENQUEUE_START,
  PRIVQ_ENQUEUE_END,
  PRIVQ_DEQUEUE_START,
  PRIVQ_DEQUEUE_END
} log_event;

#ifdef __cplusplus
extern "C"
{
#endif
  typedef struct
  {
    log_event event;
    void* p1;
    void* p2;
    struct timespec timestamp;
  } binary_info;

  void
  log_setup(int level);

  void
  log_binary(int level, log_event event, void* p1, void* p2);

  void
  logs(int level, char*, ...);

  void
  log_write();

  void
  binary_write();
#ifdef __cplusplus
}
#endif

#ifdef ENABLE_LOG
#define DEBUG_LOG(...) ; // logs(__VA_ARGS__)
#define BINARY_LOG(level, event, p1, p2) log_binary(level, event, p1, p2);
#else
#define DEBUG_LOG ;
#define BINARY_LOG ;
#endif

#endif // _DEBUG_LOG_H
