import Data
import Array
import Condition_Worker

module Main
  
  main()
    local
      i: Integer
      data: separate Data
      workers: Array [separate Condition_Worker]
      worker: separate Condition_Worker
      n: Integer
      iters: Integer
    do
      n := 32
      iters := 20000
      create data.make(2*n)
      create workers.make(2*n)

      from i := 0
      until i >= 2*n
      loop
        create worker.make(i \\ 2, iters, data)
        separate worker
          do
            worker.run()
          end
        workers.put(i, worker)
        i := i + 1
      end

      from i := 0
      until i >= 2*n
      loop
        worker := workers.item(i)
        separate worker
          require worker.done
          do end
        shutdown worker
        i := i + 1
      end

      separate data
        require data.done
        do end
      shutdown data
    end
end
