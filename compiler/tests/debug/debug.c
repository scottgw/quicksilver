#include <libqs/private_queue.h>
#include <libqs/processor.h>
#include <stdint.h>

/* %Worker = type { %separate_wrapper*, i64 } */
/* %separate_wrapper = type <{ %procStructType*, i8* }> */
/* define void @__Worker_run(%procStructType*, %Worker* nocapture) { */

struct sep {
  processor_t proc;
  void* ptr;
};

struct worker {
  struct sep* sep_data;
  int64_t sign;
};

struct data {
  int64_t value;
};

int64_t
__Data_get_value(processor_t,  struct data*);

void
__Data_incr(processor_t,  struct data*);



int num_iters = 5000;


int64_t
data_get_value_packed(struct sep* sep_data)
{
  return __Data_get_value(sep_data->proc, (struct data*)sep_data->ptr);
}


void
data_incr_packed(struct sep* sep_data)
{
  __Data_incr(sep_data->proc, (struct data*)sep_data->ptr);
}

void
worker_stub1(processor_t proc, struct worker* worker)
{  
  struct sep* sep_data = worker->sep_data;
  int64_t flag = worker->sign;

  processor_t shared = sep_data->proc;
  struct data* data = sep_data->ptr;

  void ***args;
  clos_type_t *arg_types;
  priv_queue_t q = NULL;


  for (int i = 0; i < num_iters; i++)
    {
      int64_t val;
      closure_t clos;
      q = proc_get_queue (proc, shared);

      priv_queue_lock(q, proc);

      clos =
        closure_new(data_get_value_packed,
                    closure_sint_type(),
                    1,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      *args[0] = sep_data;

      priv_queue_function(q, clos, &val, proc);

      while (val % 2 != flag)
        {
          priv_queue_unlock(q, proc);
          proc_wait_for_available(shared, proc);

          q = proc_get_queue (proc, shared);
          priv_queue_lock(q, proc);
          clos =
            closure_new(data_get_value_packed,
                        closure_sint_type(),
                        1,
                        &args,
                        &arg_types);


          arg_types[0] = closure_pointer_type();
          *args[0] = sep_data;

          priv_queue_function(q, clos, &val, proc);
        }

      clos =
        closure_new(data_incr_packed,
                    closure_void_type(),
                    1,
                    &args,
                    &arg_types);


      arg_types[0] = closure_pointer_type();
      *args[0] = sep_data;

      priv_queue_routine(q, clos, proc);
      priv_queue_unlock(q, proc);
    }
  fprintf(stderr, "%p worker finished with %d\n", proc, flag);
}


void
worker_stub2(processor_t proc, struct worker* worker)
{  
/* void worker(processor_t proc, processor_t shared, int flag)  */
/* { */

  /* processor_t shared = worker->sep_data->proc; */
  /* struct data* data = worker->sep_data->ptr; */
  int64_t flag = worker->sign;
  priv_queue_t q = NULL;



  for (int i = 0; i < num_iters; i++)
    {
      q = proc_get_queue (proc, worker->sep_data->proc);


    lock_block:
      {
        priv_queue_lock(q, proc);
      
        int64_t val;
        closure_t clos;
        void ***args1;
        clos_type_t *arg_types;

        clos =
          closure_new(__Data_get_value,
                      closure_sint_type(),
                      2,
                      &args1,
                      &arg_types);

        arg_types[0] = closure_pointer_type();
        arg_types[1] = closure_pointer_type();
        *args1[0] = worker->sep_data->proc;
        *args1[1] = worker->sep_data->ptr;

        priv_queue_function(q, clos, &val, proc);
        if (val % 2 != flag)
          {
            goto retry_block;
          }
        else
          {
            goto log_call_block;
          }
      }
    retry_block:
      priv_queue_unlock(q, proc);
      proc_wait_for_available(worker->sep_data->proc, proc);
      goto lock_block;

    log_call_block:
      {
        void ***args2;
        clos_type_t *arg_types;
        closure_t clos;
        clos =
          closure_new(__Data_incr,
                      closure_void_type(),
                      2,
                      &args2,
                      &arg_types);


        arg_types[0] = closure_pointer_type();
        arg_types[1] = closure_pointer_type();
        *args2[0] = worker->sep_data->proc;
        *args2[1] = worker->sep_data->ptr;
      
        priv_queue_routine(q, clos, proc);
        priv_queue_unlock(q, proc);
      }
    }

  fprintf(stderr, "%p worker with %d\n", proc, flag);
}
