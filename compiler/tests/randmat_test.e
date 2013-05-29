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

module Randmat_Test

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
      workers: Array[separate Randmat_Worker]
    do
      nrows := 8000
      ncols := 8000
      s := 8
      num_workers := 32
 
      create matrix.make (nrows,ncols)
      create workers.make (32)

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
          require worker.is_done() = 1
          do
            from
              ii := worker.get_start()
              iend := ii + worker.get_height()
            until ii >= iend
            loop
              from jj := 0
              until jj >= ncols
              loop
                worker.get (ii, jj) -- should store in real matrix
                jj := jj + 1
              end
              ii := ii + 1
            end      
          end
        i := i + 1
      end


      from i := 0 
      until i >= num_workers
      loop
        shutdown workers.item(i)
        i := i + 1
      end
    end

end
