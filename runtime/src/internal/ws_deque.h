#ifndef _WS_DEQUE_H
#define _WS_DEQUE_H

#include <stdint.h>
#include <stdbool.h>

#include "libqs/types.h"

#ifdef __cplusplus
extern "C" {
#endif

ws_deque_t
ws_deque_new();

void
ws_deque_free(ws_deque_t wsd);

int64_t
ws_deque_size(ws_deque_t wsd);

void
ws_deque_push_bottom(ws_deque_t wsd, void* data);

bool
ws_deque_pop_bottom(ws_deque_t wsd, void** data);

bool
ws_deque_steal(ws_deque_t wsd, void** data);

#ifdef __cplusplus
}
#endif

#endif // _WS_DEQUE_H
