#ifndef _DEBUG_LOG_H
#define _DEBUG_LOG_H

#ifdef __cplusplus
extern "C"
{
#endif

  void
  log_setup(int level);

  void
  logs(int level, char*, ...);

  void
  log_write();


#ifdef __cplusplus
}
#endif

#endif // _DEBUG_LOG_H
