-- randmat: random number generation
--
-- input:
--   nrows, ncols: the number of rows and cols
--   s: the seed
--
-- output:
--   matrix: a nrows by ncols integer matrix

import Prelude
import Array
import Randmat_Worker

module Main

  main()
    local
      ncols: Integer
      matrix: Int_Matrix
      num_workers: Integer
      height: Integer
      start: Integer
      nrows, s: Integer
      is_bench: Boolean
      i, j: Integer
      ii, jj: Integer
      iend: Integer
      worker: separate Randmat_Worker
      worker_matrix: separate Int_Matrix
      worker_start: Integer
      worker_height: Integer
      workers: Array[separate Randmat_Worker]
    do
      nrows := 10000
      ncols := 10000
      s := 0
      num_workers := {Prelude}.get_int_env("LIBQS_EXECS")

      create matrix.make (nrows, ncols)
      create workers.make (num_workers)

      from
        start := 0
        i := 0
      until i >= num_workers
      loop
        height := (nrows - start) // (num_workers - i)
        -- {Prelude}.print({Prelude}.int_to_str(height))
        if height /= 0 then
          create worker.make (start, height, ncols, s)
          separate worker
            do
              worker.fill_matrix()
            end
  
          workers.put(i, worker)
        end
          
        start := start + height
        i := i + 1
      end

      fetch_all(workers, ncols, matrix)
       
      {Prelude}.exit_with(0)
--      from i := 0 
--      until i >= num_workers
--      loop
--        shutdown workers.item(i)
--        i := i + 1
--      end
    end
  fetch_all(workers: Array[separate Randmat_Worker];
                ncols: Integer;
                matrix: Int_Matrix)
    local
      i: Integer
      worker_time: Real
    do
      worker_time := 0.0      
      from i := 0
      until i >= workers.count
      loop
        worker_time := worker_time + fetch_one(workers.item(i), ncols, matrix)
        i := i + 1
      end

      -- {Prelude}.print(matrix.to_string())
      worker_time := worker_time / {Prelude}.int_to_real(workers.count)
      {Prelude}.print_err({Prelude}.real_to_str(worker_time))
      {Prelude}.print_err("%N")
   end

  fetch_one (worker: separate Randmat_Worker;
             ncols: Integer;
             matrix: Int_Matrix): Real
    local
      i: Integer
      j: Integer
      iend: Integer
      worker_start: Integer
      worker_height: Integer
      worker_matrix: separate Int_Matrix
      worker_time: Real
   do
      separate worker
        require worker.done
        do
          worker_start := worker.start
          worker_height := worker.height
          worker_matrix := worker.matrix
          Result := worker.time
        end

      separate worker_matrix
        do
          from
            worker_matrix.height
            i := worker_start
            iend := i + worker_height
          until i >= iend
          loop
            from j := 0
            until j >= ncols
            loop
              matrix.put (j, i, worker_matrix.item (j, i - worker_start))
              j := j + 1
            end
            i := i + 1
          end
        end
    end
end
