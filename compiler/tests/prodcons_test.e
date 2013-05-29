import Int_Stack
import Array
import Producer_Worker
import Consumer_Worker

module Prodcons_Test
  
  main()
    local
      i: Integer
      stack: separate Int_Stack
      prod_workers: Array [separate Producer_Worker]
      prod_worker: separate Producer_Worker
      cons_workers: Array [separate Consumer_Worker]
      cons_worker: separate Consumer_Worker
      n: Integer
    do
      n := 32
      create stack.make()

      create prod_workers.make(n)
      from i := 0
      until i >= n
      loop
        create prod_worker.make(stack)
        separate prod_worker
          do
            prod_worker.run()
          end
        prod_workers.put(i, prod_worker)
        i := i + 1
      end

      create cons_workers.make(n)
      from i := 0
      until i >= n
      loop
        create cons_worker.make(stack)
        separate cons_worker
          do
            cons_worker.run()
          end
        cons_workers.put(i, cons_worker)
        i := i + 1
      end


      from i := 0
      until i >= n
      loop
        shutdown prod_workers.item(i)
        i := i + 1
      end

      from i := 0
      until i >= n
      loop
        shutdown cons_workers.item(i)
        i := i + 1
      end
 

      shutdown stack
    end
end
