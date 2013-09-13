import Array
import Winnow_Value_Point
import Prelude

module Winnow_Sort

  sort_val_points (points: Array[Winnow_Value_Point]): Array [Winnow_Value_Point]
    do
      sort_range(points, 0, points.count)
    end

  sort_range(points: Array[Winnow_Value_Point];
             start: Integer; final: Integer
             ): Array [Winnow_Value_Point]
    local
      part: Integer
    do
      if start < final then
        part := partition (points, start, final, start)
        sort_range (points, start, part)
        sort_range (points, part + 1, final)
      end
    end

  partition (points: Array[Winnow_Value_Point];
             start: Integer;
             final: Integer;
             pivot_idx: Integer
             ): Integer
    local
      i: Integer
      val_pt: Winnow_Value_Point
      tmp_pt: Winnow_Value_Point
      curr_pt: Winnow_Value_Point
      store_idx: Integer
    do      
      val_pt := points.item(pivot_idx)

      points.put (pivot_idx, points.item (final - 1))
      points.put (final - 1, val_pt)

      store_idx := start

      from i := start
      until i >= final - 1
      loop
        curr_pt := points.item (i)
        if curr_pt.value <= val_pt.value or curr_pt.x <= val_pt.y or curr_pt.y <= val_pt.y then --FIXME: use coords after value
          -- swap the current element with the partition
          points.put (i, points.item (store_idx))
          points.put (store_idx, curr_pt)
          
          store_idx := store_idx + 1
        end
        i := i + 1
      end

      tmp_pt := points.item (i)
      points.put (i, points.item (store_idx))
      points.put (store_idx, tmp_pt)

      Result := store_idx
    end

end
