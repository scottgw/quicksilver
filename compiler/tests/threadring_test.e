import Threadring_Worker
import Token

module Threadring_Test

  main()
    local
      worker: separate Threadring_Worker
      last_worker: separate Threadring_Worker
      first_worker: separate Threadring_Worker
      token: separate Token
      i: Integer
      n: Integer
      num_passes: Integer
    do
      n := 503
      num_passes := 50000

      create token.make()
      
      create first_worker.make (0, Void, num_passes)
      last_worker := first_worker
      
      from i := 1
      until i >= n
      loop
        create worker.make (i, last_worker, num_passes)
        last_worker := worker
        i := i + 1
      end

      separate first_worker
        do
          first_worker.set_next (worker)
          first_worker.pass(token)
        end
    end
end
