#include <fstream>
#include <iostream>
#include <stdarg.h>
#include <stdio.h>
#include <sys/time.h>
#include <tbb/concurrent_queue.h>
#include <unistd.h>

#include "libqs/debug_log.h"

extern "C"
{
  typedef struct
  {
    char* str;
    timespec timestamp;
  } log_info;

  tbb::concurrent_queue<log_info> *log_queue;
  tbb::concurrent_queue<binary_info> *binary_log;
  int log_level;

  void
  log_setup(int level)
  {
    log_queue = new tbb::concurrent_queue<log_info>();
    binary_log = new tbb::concurrent_queue<binary_info>();
    log_level = level;
  }


  void
  log_binary(int level, log_event event, void* p1, void* p2)
  {
    if (level <= log_level)
      {
        timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);

        binary_info info;

        info.timestamp = ts;
        info.event = event;
        info.p1 = p1;
        info.p2 = p2;

        binary_log->push(info);
      }    
  }

  void
  logs(int level, char* str, ...)
  {
    if (level <= log_level)
      {
        timespec ts;
        clock_gettime(CLOCK_REALTIME, &ts);

        char* tmp_str = (char*) malloc(200*sizeof(char));
        va_list args;
        va_start(args, str);
        vsnprintf(tmp_str, 200, str, args);
        va_end(args);

        log_info info;
        info.str      = tmp_str;
        info.timestamp = ts;

        log_queue->push(info);
      }
  }

  void
  binary_write()
  {
    if (log_level != 0)
      {
        std::cout << "writing binary log" << std::endl;
        std::ofstream outfile("qs_binary.log",
                              std::ios::out | std::ios::binary);
        binary_info info;
        while (binary_log->try_pop(info))
          {
            outfile.write((const char*) &info, sizeof(binary_info));
          }
        outfile.flush();
        outfile.close();
      }
    delete log_queue;
    delete binary_log;    
  }

  void
  log_write()
  {
    if (log_level != 0)
      {
        std::cout << "writing log\n";
        std::ofstream outfile;
        outfile.open("qs.log");
        outfile.precision(15);
        log_info info;
        while (log_queue->try_pop(info))
          {
            timespec t = info.timestamp;
            outfile << t.tv_sec << " " << t.tv_nsec;
            outfile << ": " << info.str;
            free(info.str);
          }
        outfile.close();
      }
    delete log_queue;
    delete binary_log;
  }
}
