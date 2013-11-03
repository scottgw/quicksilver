#ifndef _CTX_H
#define _CTX_H

#include <stdbool.h>
#include <stdint.h>

#include "../libqs/types.h"

ctx_t
ctx_new();

void
ctx_free(ctx_t ctx);

void
ctx_make(ctx_t ctx, void (*func)(void*), void* ptr);

void
ctx_set_next(ctx_t ctx, ctx_t next_ctx);

bool
ctx_save(ctx_t ctx);

uint32_t
ctx_set(ctx_t ctx);

#endif // _CTX_H
