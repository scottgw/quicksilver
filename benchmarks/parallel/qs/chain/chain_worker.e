import Array
import Int_Matrix
import Prelude
import Real_Array
import Real_Math
import Real_Matrix
import Winnow_Gatherer
import Winnow_Value_Point

class Chain_Worker

create make

  val_points: Array[Winnow_Value_Point]
  product: Real_Array
  start: Integer
  final: Integer

  nelts: Integer
  seed: Natural_32
  winnow_nelts: Integer

  num_workers: Integer

  time: Real

  make (a_num_workers: Integer;
        a_start: Integer;
        a_height: Integer;
        a_nelts: Integer;
        a_seed: Integer;
        a_winnow_nelts: Integer
       )
    do
      num_workers := a_num_workers

      start := a_start
      final := start + a_height

      nelts := a_nelts
      seed := {Prelude}.int_to_nat32(a_seed)
      winnow_nelts := a_winnow_nelts

      create val_points.make(0)
      
      time := 0.0
    end

  randmat_matrix: Int_Matrix

  start_randmat()
    local
      s, lcg_a, lcg_c, rand_max: Natural_32
      i, j: Integer
      l_time: Real
    do
      l_time := {Prelude}.get_time()
      create randmat_matrix.make(final - start, nelts)
      lcg_a := 1664525
      lcg_c := 1013904223
      rand_max := 100

      from i := start
      until i >= final
      loop
        s := seed + {Prelude}.int_to_nat32 (i)
        from j := 0
        until j >= nelts
        loop
          s := lcg_a * s + lcg_c
          randmat_matrix.put(j, i - start, {Prelude}.nat32_to_int(s \\ rand_max))
          j := j + 1
        end
        i := i + 1
      end
      -- {Prelude}.print(randmat_matrix.to_string())
      time := time + {Prelude}.get_time() - l_time
    end

  -- Threshhold
  hist: Int_Array

  start_thresh(a_sep_max: separate Int_Array; a_sep_hist: separate Int_Array)
    local
      i, j: Integer
      e: Integer
      max: Integer
      h: Integer
      l_time: Real
    do
      l_time := {Prelude}.get_time()
      create hist.make(100)
      max := 0

      from i := 0
      until i >= 100
      loop
        hist.put(i, 0)
        i := i + 1
      end

      from i := start
      until i >= final
      loop
        from j := 0
        until j >= nelts
        loop
          e        := randmat_matrix.item(j, i - start)
          hist.put(e, hist.item(e) + 1)
          max      := {Prelude}.int_max(e, max)
          j := j + 1
        end
        i := i + 1
      end
      time := time + {Prelude}.get_time() - l_time

      -- Merge hist and max
      separate a_sep_max
        do
          a_sep_max.put(0, {Prelude}.int_max(a_sep_max.item(0), max))
        end

      separate a_sep_hist
        do
          from i := 0
          until i >= 100
          loop
            h := a_sep_hist.item (i)
            a_sep_hist.put (i, h + hist.item(i))
            i := i + 1
          end      
        end
    end

  mask_matrix: Int_Matrix
  mask_count: Integer

  resume_thresh(thresh: Integer)
    local
      i: Integer
      j: Integer
      b: Integer
      l_time: Real
    do
      l_time := {Prelude}.get_time()
      create mask_matrix.make(final - start, nelts)
      mask_count := 0

      from
        i := start
      until
        i >= final
      loop
        from j := 0
        until j >= nelts
        loop
          if randmat_matrix.item (j, i - start) >= thresh then
            b := 1
          else
            b := 0
          end

          mask_count := mask_count + b
          mask_matrix.put(j, i - start, b)
          j := j + 1
        end
        i := i + 1
      end
      {Prelude}.print("Worker: finished thresh resume ")
      {Prelude}.print({Prelude}.int_to_str(mask_count))
      {Prelude}.print("%N")
      time := time + {Prelude}.get_time() - l_time
      start_winnow()
    end

  -- Winnow section
  val_points: Array [Winnow_Value_Points]
  
  start_winnow()
    local
      i, j: Integer
      l_time: Real
      value_point: Winnow_Value_Point
      val_pt_idx: Integer
    do
      l_time := {Prelude}.get_time()
      create val_points.make(mask_count)

      from
        i := start
        val_pt_idx := 0
      until
        i >= final
      loop
        from j := 0
        until j >= nelts
        loop
          if mask_matrix.item(j, i - start) = 1 then
            create value_point.make(randmat_matrix.item(j, i - start), j, i)
            val_points.put (val_pt_idx, value_point)
            val_pt_idx := val_pt_idx + 1            
          end
          j := j + 1
        end
        i := i + 1
      end

      val_points := {Winnow_Sort}.sort_val_points(val_points)

      create gather.make(winnow_nelts)
      gather.merged := val_points

      time := time + {Prelude}.get_time() - l_time
    end

  gather: Winnow_Gatherer

  merge_with (other: separate Chain_Worker)
    do
      create gather.make(winnow_nelts)
      gather.merged := val_points
      gather.fetch (other)
      val_points := gather.merged
      {Prelude}.print("Worker: merge_with done%N")
    end

  chunk()
    do
      {Prelude}.print("Worker: chunk%N")
      gather.chunk()
      x_points := gather.x_points
      y_points := gather.y_points
    end

  chunk_from(winnow_gatherer: separate Winnow_Gatherer)
    local
      i: Integer
    do
      {Prelude}.print("Worker: chunk from other%N")
      create x_points.make(winnow_nelts)
      create y_points.make(winnow_nelts)

      separate winnow_gatherer
        do
          from
            i := 0
          until
            i >= winnow_nelts
          loop
            x_points.put(i, winnow_gatherer.x_points.item(i))
            y_points.put(i, winnow_gatherer.y_points.item(i))
            i := i + 1
          end
        end

      {Prelude}.print("Worker: chunk from other finish%N")
    end

  -- Outer section
  x_points: Int_Array
  y_points: Int_Array

  start_outer(a_start: Integer;
              a_height: Integer;
              a_shared_outer_vector: separate Real_Array;
              a_shared_outer_count: separate Int_Array
             )
    local
      i: Integer
    do
      {Prelude}.print("Worker: starting outer%N")
      start := a_start
      final := start + a_height
      shared_outer_vector := a_shared_outer_vector
      shared_outer_count := a_shared_outer_count

      calc_outer()
      calc_product()
    end

  outer_matrix: Real_Matrix
  outer_vector: Real_Array
  shared_outer_vector: separate Real_Array
  shared_outer_count: separate Int_Array
  
  calc_outer()
    local
      nmax: Real
      d: Real
      i, j: Integer
      x1, y1, x2, y2: Integer
      l_time: Real
    do
      l_time := {Prelude}.get_time()
      create outer_matrix.make(final - start, winnow_nelts)
      create outer_vector.make(winnow_nelts)
      {Prelude}.print("Worker: starting outer calculation%N")
      from i := start
      until i >= final
      loop
        nmax := -1.0
        x1 := x_points.item(i - start)
        y1 := y_points.item(i - start)

        from j := 0
        until j >= winnow_nelts
        loop
          if i /= j then
            x2 := x_points.item(j)
            y2 := y_points.item(j)
            d := distance (x1, y1, x2, y2)
            outer_matrix.put (j, i - start, d)
            nmax := {Real_Math}.max (nmax, d)
          end
          j := j + 1
        end

        outer_matrix.put (i, i - start, nmax * {Real_Math}.from_int (nelts))
        outer_vector.put (i - start, distance (0, 0, x1, y1))
        i := i + 1
      end

      separate shared_outer_vector shared_outer_count
        do
          from i := start
          until i >= final
          Loop
            shared_outer_vector.put (i, outer_vector.item(i - start))
            i := i + 1
          end
          shared_outer_count.put(0, shared_outer_count.item(0) + final - start)
        end

        
      separate shared_outer_count
        require shared_outer_count.item(0) = winnow_nelts
	do
        end

      separate shared_outer_vector
        do
          from i := 0
          until i >= winnow_nelts
          loop
            outer_vector.put (i, shared_outer_vector.item(i))
            i := i + 1
          end
        end
        
      time := time + {Prelude}.get_time() - l_time
    end

  distance (x1, y1, x2, y2: Integer): Real
    local
      dx: Real
      dy: Real
    do
      dx := {Real_Math}.from_int (x2 - x1)
      dy := {Real_Math}.from_int (y2 - y1)
      Result := {Real_Math}.sqrt (dx*dx + dy*dy)
    end

  -- Product part
  product: Real_Array

  calc_product()
    local
      sum: Real
      i, j: Integer
      l_time: Real
    do
      {Prelude}.print("Worker: starting product%N")
      {Prelude}.print({Prelude}.int_to_str(final))
      l_time := {Prelude}.get_time()
      create product.make(final - start)

      from i := start
      until i >= final
      loop
        sum := 0.0
        from j := 0
        until j >= winnow_nelts
        loop
          sum := sum + outer_vector.item(j) * outer_matrix.item(j, i - start)
          j := j + 1
        end

        product.put (i - start, sum)

        i := i + 1
      end
      time := time + {Prelude}.get_time() - l_time
    end
end
