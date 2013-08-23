#ifndef __MPSC_BLOCKING_H
#define __MPSC_BLOCKING_H

#include "../libqs/types.h"

qoq_t
qoq_new(uint32_t size);

void
qoq_enqueue_wait(qoq_t q, void *data, sched_task_t stask);

void
qoq_dequeue_wait(qoq_t q, void **data, sched_task_t stask);

void
qoq_free(qoq_t q);  

#endif // __MPSC_BLOCKING_H
