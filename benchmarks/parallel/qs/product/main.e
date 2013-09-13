import Array
import Product_Worker
import Real_Array
import Real_Matrix
import Prelude

module Main

  main()
    local
      i: Integer
      j, jj: Integer
      start: Integer
      height: Integer

      n: Integer
      nelts: Integer

      vector: separate Real_Array
      matrix: separate Real_Matrix

      result_vector: Real_Array

      worker: separate Product_Worker
      workers: Array [separate Product_Worker]
    do

      nelts := 10000
      n := 32

      -- Create storage for the input and result
      create vector.make (nelts)
      create matrix.make (nelts, nelts)
      create result_vector.make (nelts)
      
      -- FIXME: fill the vector and matrix
      from i := 0
      until i >= nelts
      loop
        result_vector.put (i, 0.0)
        i := i + 1
      end
      
      -- Create worker vector
      create workers.make (n)

      -- Construct workers and call their calculation.
      from
        start := 0
        i := 0
      until
        i >= n
      loop
        height := (nelts - start) // (n - i)
        create worker.make(start, start + height, nelts, matrix, vector)
        workers.put(i, worker)

        separate worker
          do
            worker.calculate()
          end
          
        start := start + height
        i := i + 1
      end

      -- Fetch results back
      from i := 0
      until i >= n
      loop
        fetch_from_worker (workers.item(i), nelts, result_vector)
        i := i + 1
      end

      {Prelude}.exit_with (0)
      -- shutdown vector
      -- shutdown matrix
    end


  fetch_from_worker (worker: separate Product_Worker; nelts: Integer;
                    result_vector: Real_Array)
    local
      i: Integer
    do
      separate worker
        do
          i := worker.start
--          from i := worker.start
--          until i >= worker.final
--          loop
--            result_vector.put (i, result_vector.item(i) +
--              worker.prod_vector.item (i))
--            i := i + 1
--          end
        end
      -- shutdown worker
    end

  
end
