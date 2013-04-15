#ifndef __EXECUTOR_H_
#define __EXECUTOR_H_

#include <pthread.h>

#include "list.h"
#include "processor.h"

#include "sync_ops.h"

// Constructs the executor thread and adds the executor
// To the list of executors.
executor_t
make_executor();

// Join all the executors in 'executors'.
void
join_executors();

// Reschedule the currently executing processor.
void
executor_reschedule(executor_t);

// Creates 'n' executors and stores them in the 'executors' list.
void
create_executors(list_t list, int n);

#endif // __EXECUTOR_H_
