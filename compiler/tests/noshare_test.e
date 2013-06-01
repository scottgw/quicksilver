import Array
import Noshare_Worker

module Noshare_Test
  
  main()
    local
      i: Integer
      workers: Array [separate Noshare_Worker]
      worker: separate Noshare_Worker
      n: Integer
    do
      n := 32
      create workers.make(n)

      from i := 0
      until i >= n
      loop
        create worker.make()
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
    end
end
