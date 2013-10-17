import Array
import Chain_Worker
import Int_Array
import Winnow_Sort
import Winnow_Value_Points

class Winnow_Gatherer

create make

  winnow_nelts: Integer
  chunked_points: Winnow_Value_Points

  merged: Winnow_Value_Points
  num_merged: Integer

  time: Real

  make(a_winnow_nelts: Integer)
    do
      winnow_nelts := a_winnow_nelts
      num_merged := 0
      create merged.make(0)
    end

    -- Fetch the points from the workers and merge them (workers should
    -- take care to have already sorted their individual pieces.
    --
    -- After merging, only take `winnow_nelts' from the merged lists
    -- and store them in the x and y points arrays.
  fetch(worker: separate Chain_Worker)
    local
      other_i: Integer
      new_merged_i: Integer
      merged_i: Integer

      other_points: Winnow_Value_Points
      new_merged: Winnow_Value_Points
      val_end: Integer
    do

      {Prelude}.print("Winnow_Gatherer: starting fetch%N")
      other_points := copy_points_from_worker(worker)
      {Prelude}.print("Winnow_Gatherer: starting merge%N")
      from
        time := {Prelude}.get_time()
        new_merged_i := 0
        merged_i := 0
        other_i := 0
        create new_merged.make (merged.count + other_points.count)
      until
        new_merged_i >= new_merged.count
      loop
        if merged_i < merged.count and 
           other_i < other_points.count then
          if {Winnow_Sort}.cmp_pt(merged_i, other_i, merged, other_points) then
            new_merged.put_from_other (new_merged_i, merged, merged_i)
            merged_i := merged_i + 1
          else
            new_merged.put_from_other (new_merged_i, other_points, other_i)
            other_i := other_i + 1
          end
        elseif merged_i < merged.count then
          new_merged.put_from_other (new_merged_i, merged, merged_i)
          merged_i := merged_i + 1
        else
          new_merged.put_from_other (new_merged_i, other_points, other_i)
          other_i := other_i + 1
        end
        
        new_merged_i := new_merged_i + 1

      end
      {Prelude}.print("Winnow_Gatherer: finished merge%N")

      time := {Prelude}.get_time() - time
      {Prelude}.print("Winnow_Gatherer: time - ")
      {Prelude}.print({Prelude}.real_to_str(time))
      {Prelude}.print("%N")

      num_merged := num_merged + 1
      merged := new_merged
    end

  copy_points_from_worker(worker: separate Chain_Worker): Winnow_Value_Points
    local
      other_i: Integer
      other_points: Winnow_Value_Points
      x, y, v: Integer
      sep_points: separate Winnow_Value_Points
    do
      {Prelude}.print("Winnow_Gatherer: locking other worker%N")
      if worker = Void then
        {Prelude}.print("Winnow_Gatherer: void worker%N")
      end
      separate worker
        do
          {Prelude}.print("Winnow_Gatherer: copy copy%N")
          create other_points.make(worker.val_points.count)
          from
            other_i := 0
          until
            other_i >= other_points.count
          loop
            v := worker.val_points.item_v (other_i)
            x := worker.val_points.item_x (other_i)
            y := worker.val_points.item_y (other_i)
            other_points.put (other_i, v, x, y)
            other_i := other_i + 1
          end
        end
      Result := other_points
    end
  
  chunk()
    local
      chunk: Integer
      i: Integer
    do
      create chunked_points.make(winnow_nelts)

      -- Take every nth element from the sorted list to store.
      chunk := merged.count // winnow_nelts

      {Prelude}.print("Winnow_Gatherer: starting chunking ")
      {Prelude}.print({Prelude}.int_to_str(chunk))
      {Prelude}.print("%N")
      
      from
        i := 0
      until
        i >= winnow_nelts
      loop
        chunked_points.put_from_other (i, merged, i * chunk)
        i := i + 1
      end
      {Prelude}.print("Winnow_Gatherer: finished chunking ")
    end
end
