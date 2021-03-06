import Array

import Input_Creator
import Winnow_Value_Point
import Winnow_Point
import Winnow_Sort
import Winnow_Worker

module Main

  main()
    local
      i: Integer
      j, jj: Integer
      start: Integer
      height: Integer

      rows: Integer
      cols: Integer
      n: Integer
      nelts: Integer

      inp: separate Input_Creator
      sep_mask: separate Int_Matrix
      sep_matrix: separate Int_Matrix

      worker: separate Winnow_Worker
      workers: Array [separate Winnow_Worker]

      val_pt_count: Integer
      val_pt: Winnow_Value_Point
      sep_val_points: separate Array [Winnow_Value_Point]
      val_points: Array [Winnow_Value_Point]
      x_points: Int_Array
      y_points: Int_Array

      time: Real
    do
      cols := 10000
      rows := 10000
      nelts := 1000
      n := {Prelude}.get_int_env("LIBQS_EXECS")

      -- Create mask and matrix
      create inp.make(cols)
      separate inp
        do
          sep_mask := inp.mask
          sep_matrix := inp.matrix
        end

      create workers.make (n)

      -- Construct and call the unmasked call.
      from
        start := 0
        i := 0
      until
        i >= n
      loop
        height := (rows - start) // (n - i)
        create worker.make(start, start + height, cols, sep_mask, sep_matrix)
        workers.put(i, worker)

        separate worker
          do
            worker.gather_unmasked()
          end
          
        start := start + height
        i := i + 1
      end

      val_points := merge_val_points (workers)
      time := {Prelude}.get_time();
      sort_list (val_points)

--      shutdown sep_mask
--      shutdown sep_matrix

      create x_points.make (nelts)
      create y_points.make (nelts)
      
      -- Pull out every kth element from the sorted list, throw away 
      -- the 'value' part.
      chunk_points (val_points, nelts, x_points, y_points)
      time := get_time(workers) / {Prelude}.int_to_real(n) +
             {Prelude}.get_time() - time
      {Prelude}.print_err({Prelude}.real_to_str(time))
      {Prelude}.print_err("%N")
      {Prelude}.exit_with(0) 
    end
  
  get_time(workers: Array[separate Winnow_Worker]): Real
    local
      time: Real
      i: Integer
      worker: separate Winnow_Worker
    do
      time := 0.0
      from i := 0
      until i >= workers.count
      loop
        worker := workers.item(i)
        separate worker
          do
            time := time + worker.time
          end
        i := i + 1
      end
      Result := time
    end


  merge_val_points(workers: Array[separate Winnow_Worker]):
    Array [Winnow_Value_Point]
    local
      i: Integer
      j, jj: Integer
		n: Integer 
      val_pt_count: Integer

      val_pt: Winnow_Value_Point
      val_points: Array [Winnow_Value_Point]

      worker: separate Winnow_Worker
      sep_val_points: separate Array [Winnow_Value_Point]
    do
      -- Calculate the total number of value points.
      val_pt_count := 0

      from i := 0
      until i >= workers.count
      loop
        worker := workers.item (i)
        separate worker
          do
           sep_val_points := worker.val_points
          end
        separate sep_val_points
          do
            val_pt_count := val_pt_count + sep_val_points.count
         end
        i := i + 1
      end

      create val_points.make (val_pt_count)
 
      -- Fetch the value points from each array
      from
        i := 0
        j := 0
      until
        i >= workers.count
      loop
        worker := workers.item (i)
        separate worker
          do
            sep_val_points := worker.val_points
          end
        separate sep_val_points
          do
				 from
					 jj := 0
					 n := sep_val_points.count
            until jj >= n
            loop
              create val_pt.make (sep_val_points.item (jj).value,
                                  sep_val_points.item (jj).x,
                                  sep_val_points.item (jj).y)
              val_points.put (j, val_pt)
              j := j + 1
              jj := jj + 1
            end
          end
        i := i + 1
        -- shutdown worker  
      end
      
      Result := val_points
    end
  
  sort_list(val_points: Array[Winnow_Value_Point])
    do
      {Winnow_Sort}.sort_val_points (val_points)
    end

  chunk_points (val_points: Array [Winnow_Value_Point]; nelts: Integer;
                x_points: Int_Array; y_points: Int_Array)
    local
      n: Integer
      i: Integer
      val_pt: Winnow_Value_Point
      index: Integer
      chunk: Integer
    do
      n := val_points.count
      chunk := n // nelts
      
      from i := 1
      until i > nelts
      loop
        index := (i - 1) * chunk + 1
        val_pt := val_points.item (index)
        x_points.put (i, val_pt.x)
        y_points.put (i, val_pt.y)

        i := i + 1
      end
    end
  
end

