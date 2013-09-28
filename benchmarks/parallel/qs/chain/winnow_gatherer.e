import Array
import Chain_Worker
import Int_Array
import Winnow_Sort
import Winnow_Value_Point

class Winnow_Gatherer

create make

  winnow_nelts: Integer
  x_points: Int_Array
  y_points: Int_Array

  merged: Array [Winnow_Value_Point]
  num_merged: Integer

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

      other_points: Array [Winnow_Value_Point]
      new_merged: Array [Winnow_Value_Point]
      val_end: Integer

      merged_pt: Winnow_Value_Point
      other_pt: Winnow_Value_Point
    do
      {Prelude}.print("Winnow_Gatherer: starting fetch%N")
      other_points := copy_points_from_worker(worker)
      {Prelude}.print("Winnow_Gatherer: starting merge%N")
      from
        new_merged_i := 0
        merged_i := 0
        other_i := 0
        create new_merged.make (merged.count + other_points.count)
      until
        new_merged_i >= new_merged.count
      loop
        if merged_i < merged.count then
          merged_pt := merged.item(merged_i)
        end
        
        if other_i < other_points.count then
          other_pt := other_points.item(other_i)
        end

        if merged_i < merged.count and 
          other_i < other_points.count then
          if {Winnow_Sort}.cmp_pt(merged_pt, other_pt) then
            new_merged.put (new_merged_i, merged_pt)
            merged_i := merged_i + 1
          else
            new_merged.put (new_merged_i, other_pt)
            other_i := other_i + 1
          end
        elseif merged_i < merged.count then
          new_merged.put(new_merged_i, merged_pt)
          merged_i := merged_i + 1
        else
          new_merged.put(new_merged_i, other_pt)
          other_i := other_i + 1
        end
        
        new_merged_i := new_merged_i + 1

      end
      {Prelude}.print("Winnow_Gatherer: finished merge%N")
      num_merged := num_merged + 1
      merged := new_merged
    end

  copy_points_from_worker(worker: separate Chain_Worker): Array [Winnow_Value_Point]
    local
      other_i: Integer
      other_points: Array [Winnow_Value_Point]
      other_pt: Winnow_Value_Point
      x, y, v: Integer
      sep_points: separate Array [Winnow_Value_Point]
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
            v := worker.val_points.item (other_i).value
            x := worker.val_points.item (other_i).x
            y := worker.val_points.item (other_i).y
            create other_pt.make (v, x, y)
            other_points.put(other_i, other_pt)
            other_i := other_i + 1
          end
        end
      Result := other_points
    end
  
  chunk()
    local
      chunk: Integer
      i: Integer
      pt: Winnow_Value_Point
    do
      create x_points.make(winnow_nelts)
      create y_points.make(winnow_nelts)

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
        pt := merged.item(i*chunk)
        x_points.put(i, pt.x)
        y_points.put(i, pt.y)
        i := i + 1
      end
      {Prelude}.print("Winnow_Gatherer: finished chunking ")
    end
end
