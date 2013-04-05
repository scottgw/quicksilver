#ifndef __EXECUTOR_H_
#define __EXECUTOR_H_

#include <glib.h>
#include <pthread.h>

#include "work_list.h"
#include "processor.h"

work_list_t work;
GList *executors;
processor_t *current_proc;

typedef struct
{
  pthread_t thread;
} executor_t;

// Constructs the executor thread and adds the executor
// To the list of executors.
executor_t*
make_executor();

// Join all the executors in 'executors'.
void
join_executors();

// Creates 'n' executors and stores them in the 'executors' list.
void
create_executors(int n);

#endif // __EXECUTOR_H_
