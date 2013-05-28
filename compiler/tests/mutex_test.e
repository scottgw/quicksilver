import Data
import Array
import Mutex_Worker

module Mutex_Test
  
  main()
    local
      i: Integer
      data: separate Data
      workers: Array [separate Mutex_Worker]
      worker: separate Mutex_Worker
      n: Integer
    do
      n := 32
      create data.make()
      create workers.make(n)

      from i := 0
      until i >= n
      loop
        create worker.make(data)
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
