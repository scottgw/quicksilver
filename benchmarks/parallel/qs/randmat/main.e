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
      s := 8
      num_workers := 32
 
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

      from i := 0
      until i >= num_workers
      loop
        worker := workers.item(i)
        separate worker
          require worker.done
          do
            worker_start := worker.start
            worker_height := worker.height
            worker_matrix := worker.matrix
          end

        separate worker_matrix
          do
            ii := worker_start
--            from
--              ii := worker_start
--              iend := ii + worker_height
--            until ii >= iend
--            loop
--              from jj := 0
--              until jj >= ncols
--              loop
--                matrix.put (jj, ii, worker_matrix.item (jj, ii))
--                jj := jj + 1
--              end
--              ii := ii + 1
--            end
          end
        i := i + 1
      end
        
      {Prelude}.exit_with(0)
--      from i := 0 
--      until i >= num_workers
--      loop
--        shutdown workers.item(i)
--        i := i + 1
--      end
    end

end