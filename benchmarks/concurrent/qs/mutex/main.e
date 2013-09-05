import Data
import Array
import Mutex_Worker

module Main
  
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
        worker := workers.item(i)
        separate worker
          do
            worker.data
          end
        i := i + 1
      end
      {Prelude}.exit_with(0)
    end
end
