import Array
import Int_Array
import Prelude
import Chain_Worker
import Winnow_Value_Point
import Winnow_Sort

module Main

  main()
    local
      nelts: Integer
      s: Integer
      percent: Integer
      winnow_nelts: Integer

      num_workers: Integer
      workers: Array[separate Chain_Worker]

      height: Integer
      start: Integer
      i, j: Integer
      worker: separate Chain_Worker

      -- Result of winnow stage:
      x_points: Int_Array
      y_points: Int_Array
    do
      nelts := 10000
      s := 0
      percent := 1
      winnow_nelts := 1000

      num_workers := {Prelude}.get_int_env("LIBQS_EXECS")
      create workers.make (num_workers)

      from
        start := 0
        i := 0
      until i >= num_workers
      loop
        height := (nelts - start) // (num_workers - i)

        create worker.make (start, height, nelts, s)
        separate worker
          do
            worker.start_randmat()
          end
  
        workers.put(i, worker)
          
        start := start + height
        i := i + 1
      end

      -- For winnow
      fetch_winnow(workers, winnow_nelts, x_points, y_points)
--    outer_send_points(workers, x_points, y_points)
--
--    -- For product
--    fetch_product_results(workers, ncols, matrix)
       
      {Prelude}.exit_with(0)
    end

  fetch_winnow(workers: Array[separate Chain_Worker];  
               winnow_nelts: Integer;
               x_points: Int_Array;
               y_points: Int_Array)
    local
      i: Integer
      ii: Integer
      j: Integer
      jj: Integer
      iend: Integer

      worker: separate Chain_Worker
      worker_start: Integer
      worker_height: Integer
      worker_matrix: separate Int_Matrix

      val_pt_count: Integer

      sep_i: Integer
      new_merged_i: Integer
      merged_i: Integer

      sep_points: separate Array [Winnow_Value_Point]
      merged: Array [Winnow_Value_Point]
      new_merged: Array [Winnow_Value_Point]
      val_end: Integer

      merged_pt: Winnow_Value_Point
      sep_pt: Winnow_Value_Point
    do
      val_pt_count := 0

      from i := 0
      until i >= workers.count
      loop
        worker := workers.item (i)
        separate worker
          do
           sep_points := worker.val_points
          end
        separate sep_points
          do
            val_pt_count := val_pt_count + sep_points.count
         end
        i := i + 1
      end

      create merged.make (0)
 
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
            sep_points := worker.val_points
          end
        separate sep_points
          do
            from
              new_merged_i := 0
              merged_i := 0
              sep_i := 0
              create new_merged.make (merged.count + sep_points.count)
            until
              new_merged_i >= new_merged.count
            loop
              if merged_i < merged.count then
                merged_pt := merged.item(merged_i)
              end
              
              if sep_i < sep_points.count then
                create sep_pt.make (sep_points.item (sep_i).value,
                                    sep_points.item (sep_i).x,
                                    sep_points.item (sep_i).y)
              end

              if merged_i < merged.count and 
                sep_i < sep_points.count then
                if {Winnow_Sort}.cmp_pt(merged_pt, sep_pt) then
                  new_merged.put (new_merged_i, merged_pt)
                  merged_i := merged_i + 1
                else
                  new_merged.put (new_merged_i, sep_pt)
                  sep_i := sep_i + 1
                end
              elseif merged_i < merged.count then
                new_merged.put(new_merged_i, merged_pt)
                merged_i := merged_i + 1
              else
                new_merged.put(new_merged_i, merged_pt)
                sep_i := sep_i + 1
              end

              new_merged_i := new_merged_i + 1
            end
          end
        merged := new_merged
        i := i + 1
      end
      
      -- Result := merged
    end


end    