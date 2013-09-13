import Array
import Int_Array
import Outer_Worker
import Real_Array
import Real_Matrix


module Main

  main()
    local
      i: Integer
      j, jj: Integer
      start: Integer
      height: Integer

      n: Integer
      nelts: Integer


      result_vector: Real_Array
      result_matrix: Real_Matrix

      worker: separate Outer_Worker
      workers: Array [separate Outer_Worker]

      x_points: separate Int_Array
      y_points: separate Int_Array
    do
      nelts := 10000
      n := 32

      -- Create storage for the results
      create result_vector.make (nelts)
      create result_matrix.make (nelts, nelts)
      
      -- Create x and y point vectors
      create x_points.make (nelts)
      create y_points.make (nelts)
      -- fill_points (x_points, nelts)
      -- fill_points (y_points, nelts)

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
        create worker.make(start, start + height, nelts, x_points, y_points)
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
        fetch_from_worker (workers.item(i), nelts,
                           result_matrix, result_vector)
        i := i + 1
      end
      {Prelude}.exit_with (0)      
      shutdown x_points
      shutdown y_points
    end

  fetch_from_worker (worker: separate Outer_Worker; nelts: Integer;
                    result_matrix: Real_Matrix; result_vector: Real_Array)
    local
      i: Integer
      j: Integer
    do
      separate worker
        do
          from i := worker.start
          until True -- i >= worker.final
          loop
--            from j := 0
--            until j >= nelts
--            loop
--              result_matrix.put (j, i, worker.matrix.item (j, i))
--              j := j + 1
--            end
--            result_vector.put (i, worker.vector.item (i))
--            i := i + 1
          end
        end
--      shutdown worker
    end

  fill_points(points: separate Int_Array; nelts: Integer)
    local
      i: Integer
    do
      separate points
        do
          from i := 0
          until i >= nelts
          loop
            points.put (i, 0)
            i := i + 1
          end
        end
    end
  
end

