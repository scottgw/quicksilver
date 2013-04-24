#include <assert.h>
#include <ucontext.h>
#include <stdlib.h>

#include "ctx.h"

struct ctx
{
  ucontext_t ctx;
};


static
uint32_t
ctx_get(ctx_t ctx)
{
  return getcontext(&ctx->ctx);
}


ctx_t
ctx_new()
{
  ctx_t ctx = (ctx_t)malloc(sizeof(struct ctx));
  ctx_get(ctx);
  return ctx;
}

void
ctx_make(ctx_t ctx, void (*func)(void*), void* ptr)
{
  makecontext(&ctx->ctx, (void (*)())func, 1, ptr);
}

void
ctx_set_next(ctx_t ctx, ctx_t next_ctx)
{
  ctx->ctx.uc_link = &next_ctx->ctx;
}

bool
ctx_save(ctx_t ctx)
{
  volatile int flag = true;
  assert (ctx_get(ctx) == 0);
  if (flag)
    {
      flag = false;
    }
  else
    {
      flag = true;
    }
  return !flag;
}

uint32_t
ctx_set(ctx_t ctx)
{
  return setcontext(&ctx->ctx);
}

void
ctx_set_stack_ptr(ctx_t ctx, void* ptr)
{
  ctx->ctx.uc_stack.ss_sp = ptr;
}

void
ctx_set_stack_size(ctx_t ctx, size_t sz)
{
  ctx->ctx.uc_stack.ss_size = sz;
}
