#include <gtest/gtest.h>

#include <libqs/sync_ops.h>
#include <libqs/processor.h>
#include <libqs/private_queue.h>
#include <libqs/closure.h>

void
root_func(processor_t proc)
{
}

TEST(Process, BasicRoot)
{
  sync_data_t sync_data = sync_data_new (1);
  volatile processor_t proc = proc_new_root (sync_data, root_func);

  sync_data_create_executors(sync_data, 1);

  sync_data_join_executors(sync_data);
  sync_data_free(sync_data);
}

processor_t *processors;
int num_each;
int num_iters;
int x;
int num_finished;

void
action(processor_t proc)
{
  x++;
}

void
worker(processor_t proc, processor_t shared)
{
  void ***args;
  clos_type_t *arg_types;
  priv_queue_t q = NULL;
  assert(proc->stask.executor != NULL);
  for (int i = 0; i < num_iters; i++)
    { 
      q = proc_get_queue(proc, shared);

      closure_t clos =
        closure_new((void*)action,
                    closure_void_type(),
                    1,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      
      *args[0] = shared;

      priv_queue_lock(q, proc);
      priv_queue_routine(q, clos, proc);
      priv_queue_unlock(q, proc);
    }

  priv_queue_shutdown(q, proc);

  if( __sync_add_and_fetch(&num_finished, 1) == num_each)
    {
      proc_shutdown(shared, proc);
    }
}

void
root_create(processor_t proc)
{
  processors = (processor_t*) malloc((num_each + 1) * sizeof(processor_t));
  processor_t shared = proc_new_from_other(proc);
  processors[num_each] = shared;
  for (int i = 0; i < num_each; i++)
    {
      processor_t worker_proc = proc_new_from_other(proc);
      processors[i] = worker_proc;
      priv_queue_t q = proc_get_queue(proc, worker_proc);
      assert(worker_proc->stask.executor != NULL);
      void ***args;
      clos_type_t *arg_types;
 
      closure_t clos =
        closure_new((void*)worker,
                    closure_void_type(),
                    2,
                    &args,
                    &arg_types);
      
      arg_types[0] = closure_pointer_type();
      arg_types[1] = closure_pointer_type();
      
      *args[0] = worker_proc;
      *args[1] = shared;

      priv_queue_lock(q, proc);
      priv_queue_routine(q, clos, proc);
      priv_queue_unlock(q, proc);

      priv_queue_shutdown(q, proc);
      proc_shutdown(worker_proc, proc);
    }
  proc_deref_priv_queues(proc);
}

void
shared_test(int workers, int iterations, int execs, void (*func)(processor_t))
{
  num_finished = 0;
  x = 0;
  num_iters = iterations;
  num_each = workers;

  sync_data_t sync_data = sync_data_new (workers + 2);
  volatile processor_t proc = proc_new_root (sync_data, func);

  sync_data_create_executors(sync_data, execs);

  sync_data_join_executors(sync_data);
  sync_data_free(sync_data);
  free(processors);
}

// FIXME: There's a scheduling problem with just 1 executor.
// This should be investigated, but isn't super urgent at the moment.
TEST(Process, DISABLED_BasicLogging1Exec)
{
  shared_test(2, 2000, 1, root_create);
}

TEST(Process, BasicLogging2Exec)
{
  shared_test(2, 2000, 2, root_create);
}

void
wait_worker(processor_t proc, processor_t shared, uint64_t flag)
{

  void ***args;
  clos_type_t *arg_types;
  priv_queue_t q = NULL;
  assert(proc->stask.executor != NULL);
  for (int i = 0; i < num_iters; i++)
    {
      int val;
      closure_t clos;
      q = proc_get_queue (proc, shared);
      assert(proc->stask.executor != NULL);
      priv_queue_lock(q, proc);
      assert(proc->stask.executor != NULL);
      priv_queue_sync(q, proc);
      assert(proc->stask.executor != NULL);

      priv_queue_set_in_wait(q);
      val = x;

      while (val % 2 != flag)
        {
          priv_queue_unlock(q, proc);

          proc_wait_for_available(shared, proc);

          q = proc_get_queue (proc, shared);

          priv_queue_lock(q, proc);
          priv_queue_sync(q, proc);

	  priv_queue_set_in_wait(q);
          val = x;
        }

      priv_queue_set_in_body(q);

      clos =
        closure_new((void*)action,
                    closure_void_type(),
                    1,
                    &args,
                    &arg_types);

      arg_types[0] = closure_pointer_type();
      *args[0] = shared;

      priv_queue_routine(q, clos, proc);
      priv_queue_unlock(q, proc);
    }

  priv_queue_shutdown(q, proc);

  if( __sync_add_and_fetch(&num_finished, 1) == 2*num_each)
    {
      proc_shutdown(shared, proc);
    }  
}

void
root_wait(processor_t proc)
{
  processors = (processor_t*) malloc((num_each + 1) * sizeof(processor_t));
  processor_t shared = proc_new_from_other(proc);
  processors[num_each] = shared;
  for (int i = 0; i < num_each; i++)
    {
      processor_t worker_proc = proc_new_from_other(proc);
      processors[i] = worker_proc;
      priv_queue_t q = proc_get_queue(proc, worker_proc);
      int64_t flag = i % 2 == 0;
      
      void ***args;
      clos_type_t *arg_types;
 
      closure_t clos =
        closure_new((void*)wait_worker,
                    closure_void_type(),
                    3,
                    &args,
                    &arg_types);
      
      arg_types[0] = closure_pointer_type();
      arg_types[1] = closure_pointer_type();
      arg_types[2] = closure_sint_type();

      *args[0] = worker_proc;
      *args[1] = shared;
      *args[2] = (void*)flag;

      priv_queue_lock(q, proc);
      priv_queue_routine(q, clos, proc);
      priv_queue_unlock(q, proc);

      priv_queue_shutdown(q, proc);

      proc_shutdown(worker_proc, proc);
    }
  proc_deref_priv_queues(proc);  
}


TEST(Process, BasicWaitCondition)
{
  shared_test(2, 4000, 2, root_wait);
}

int
main(int argc, char **argv)
{
  ::testing::InitGoogleTest(&argc, argv);
  return RUN_ALL_TESTS();
}
