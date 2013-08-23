#ifndef __MPSC_BLOCKING_H
#define __MPSC_BLOCKING_H

#include "types.h"

qo_queue_t
qo_q_new(uint32_t size);

void
qo_q_enqueue_wait(qo_queue_t q, void *data, sched_task_t stask);

void
qo_q_dequeue_wait(qo_queue_t q, void **data, sched_task_t stask);

void
qo_q_free(qo_queue_t q);  

#endif // __MPSC_BLOCKING_H
