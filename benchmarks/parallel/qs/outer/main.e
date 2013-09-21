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

      time: Real
    do
      nelts := 10000
      n := {Prelude}.get_int_env("LIBQS_EXECS")

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

      time := 0.0
      -- Fetch results back
      from i := 0
      until i >= n
      loop
        time := time + 
           fetch_from_worker (workers.item(i), nelts,
                              result_matrix, result_vector)
        i := i + 1
      end
      time := time / {Prelude}.int_to_real(n)
      {Prelude}.print_err({Prelude}.real_to_str(time))
      {Prelude}.print_err("%N")
      {Prelude}.exit_with (0)      
      shutdown x_points
      shutdown y_points
    end

  fetch_from_worker (
      worker: separate Outer_Worker;
      nelts: Integer;
      result_matrix: Real_Matrix;
      result_vector: Real_Array): Real
    local
      i: Integer
      j: Integer
      start: Integer
      final: Integer
      sep_mat: separate Real_Matrix
      sep_vec: separate Real_Array
    do
      separate worker
        do
          sep_mat := worker.matrix
          sep_vec := worker.vector
          start := worker.start
          final := worker.final
          Result := worker.time
        end

      separate sep_mat sep_vec
        do
          from i := start
          until i >= final
          loop
            from j := 0
            until j >= nelts
            loop
              result_matrix.put (j, i, sep_mat.item (j, i - start))
              j := j + 1
            end
            result_vector.put (i, sep_vec.item (i - start))
            i := i + 1
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

