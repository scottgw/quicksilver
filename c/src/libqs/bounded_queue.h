#ifndef _BOUNDED_QUEUE_H
#define _BOUNDED_QUEUE_H

#include <stdbool.h>
#include <stdint.h>

#include "types.h"

bounded_queue_t
bqueue_new(uint32_t size);

void
bqueue_free(bounded_queue_t q);

void
bqueue_use(bounded_queue_t q);

bool
bqueue_enqueue(bounded_queue_t q, void *data, sched_task_t stask);

void
bqueue_enqueue_wait(bounded_queue_t q, void *data, sched_task_t stask);

bool
bqueue_dequeue(bounded_queue_t q, void **data);

void
bqueue_dequeue_wait(bounded_queue_t q, void **data, sched_task_t stask);


#endif // _BOUNDED_QUEUE_H
