#ifndef _SPSC_QUEUE_H
#define _SPSC_QUEUE_H

#include <stdbool.h>
#include <stdint.h>

#include "types.h"

spsc_queue_t
spsc_new(uint32_t size);

void
spsc_free(spsc_queue_t q);

void
spsc_enqueue_wait(spsc_queue_t q, void *data, sched_task_t stask);

void
spsc_dequeue_wait(spsc_queue_t q, void **data, sched_task_t stask);


#endif // _SPSC_QUEUE_H
