import Array

import Winnow_Value_Point
import Winnow_Point
import Winnow_Sort
import Winnow_Worker

module Winnow_Test

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
    do
      cols := 800
      rows := 800
      nelts := 80
      n := 32

      -- Create mask and matrix
      sep_mask := load_mask (cols, rows)
      sep_matrix := load_matrix (cols, rows)
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

      -- Reconstruct a single array of value points
      val_points := merge_val_points (workers)
      sort_list (val_points)

      shutdown sep_mask
      shutdown sep_matrix

      create x_points.make (nelts)
      create y_points.make (nelts)
      
      -- Pull out every kth element from the sorted list, throw away 
      -- the 'value' part.
      chunk_points (val_points, nelts, x_points, y_points)
      
    end

  
  load_matrix (cols: Integer; rows: Integer): separate Int_Matrix
    local
      i: Integer
      j: Integer
      sep_matrix: separate Int_Matrix
    do
      create sep_matrix.make (cols, rows)

      separate sep_matrix
        do
          from i := 0
          until i >= rows
          loop
            from j := 0
            until j >= cols
            loop
              sep_matrix.put (j, i, i*j)
              j := j + 1
            end
            i := i + 1
          end
        end

      Result := sep_matrix
    end

  load_mask (cols: Integer; rows: Integer): separate Int_Matrix
    local
      i: Integer
      j: Integer
      sep_mask: separate Int_Matrix
    do
      create sep_mask.make (cols, rows)

      separate sep_mask
        do
          from i := 0
          until i >= rows
          loop
            from j := 0
            until j >= cols
            loop
              if ((i * j) \\ (cols + 1)) = 1 then
                sep_mask.put (j, i, 1)
              else
                sep_mask.put (j, i, 0)
              end
              j := j + 1
            end
            i := i + 1
          end
        end

      Result := sep_mask
    end
  
  merge_val_points(workers: Array[separate Winnow_Worker]):
    Array [Winnow_Value_Point]
    local
      i: Integer
      j, jj: Integer

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
            val_pt_count := val_pt_count + worker.val_points.count
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
            separate sep_val_points
              do
                from jj := 0
                until jj >= sep_val_points.count
                loop
                  create val_pt.make (sep_val_points.item (jj).value,
                                      sep_val_points.item (jj).x,
                                      sep_val_points.item (jj).y)
                  val_points.put (j, val_pt)
                  j := j + 1
                  jj := jj + 1
                end
              end
          end
        i := i + 1
        shutdown worker  
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

