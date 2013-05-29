import Data
import Array
import Condition_Worker

module Condition_Test
  
  main()
    local
      i: Integer
      data: separate Data
      workers: Array [separate Condition_Worker]
      worker: separate Condition_Worker
      n: Integer
    do
      n := 64
      create data.make()
      create workers.make(n)

      from i := 0
      until i >= n
      loop
        create worker.make(i \\ 2, data)
        separate worker
          do
            worker.run()
          end
        workers.put(i, worker)
        i := i + 1
      end

      from i := 0
      until i >= n
      loop
        shutdown workers.item(i)
        i := i + 1
      end
      shutdown data
    end
end
