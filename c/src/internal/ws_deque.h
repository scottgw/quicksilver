#ifndef _WS_DEQUE_H
#define _WS_DEQUE_H

#include <stdint.h>
#include <stdbool.h>

#include "libqs/../libqs/types.h"

ws_deque_t
ws_deque_new();

int64_t
ws_deque_size(ws_deque_t wsd);

void
ws_deque_push_bottom(ws_deque_t wsd, void* data);

bool
ws_deque_pop_bottom(ws_deque_t wsd, void** data);

bool
ws_deque_steal(ws_deque_t wsd, void** data);

#endif // _WS_DEQUE_H
