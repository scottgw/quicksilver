import Threadring_Worker
import Token

module Main

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
      num_passes := 50000000

      create token.make(num_passes)
      
      create first_worker.make (0, Void)
      last_worker := first_worker
      
      from i := 1
      until i >= n
      loop
        create worker.make (i, last_worker)
        last_worker := worker
        separate worker do worker.run() end
        i := i + 1
      end

      separate first_worker
        do
          first_worker.set_next (worker)
          first_worker.pass (num_passes)
        end
    end
end
