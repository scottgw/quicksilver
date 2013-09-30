import Winnow_Value_Points
import Prelude

module Winnow_Sort

  sort_val_points (points: Winnow_Value_Points): Winnow_Value_Points
    do
      {Prelude}.print({Prelude}.int_to_str(points.count))
      {Prelude}.print("%N")
 
      sort_range(points, 0, points.count)
    end

  sort_range(points: Winnow_Value_Points;
             start: Integer; final: Integer
             ): Winnow_Value_Points
    local
      part: Integer
    do
--      {Prelude}.print({Prelude}.int_to_str(start))
--      {Prelude}.print("%N")
--      {Prelude}.print({Prelude}.int_to_str(final))
--      {Prelude}.print("%N")
      if start < final then
        part := partition (points, start, final)
        sort_range (points, start, part)
        sort_range (points, part + 1, final)
      end
    end

  cmp_pt(idx1, idx2: Integer; a1, a2: Winnow_Value_Points): Boolean
    do
      if a1.item_v (idx1) <= a2.item_v (idx2) then
        Result := True
      elseif a1.item_v (idx1) = a2.item_v (idx2) then
        if a1.item_x (idx1) <= a2.item_x (idx2) then
          Result := True
        elseif a1.item_x (idx1) = a2.item_x (idx2) then
          Result := a1.item_y (idx1) <= a2.item_y (idx2)
        else
          Result := False
        end
      else
        Result := False
      end
    end

  partition (points: Winnow_Value_Points;
             start: Integer;
             final: Integer
             ): Integer
    local
      i: Integer
      left: Integer
      right: Integer
      pivot_idx: Integer
    do
      pivot_idx := start + (final - start) // 2

      from
        left := start
        right := final - 1
      until left >= right
      loop
        if (not cmp_pt(left, pivot_idx, points, points)) and
           cmp_pt(pivot_idx, right, points, points) then
          points.swap (left, right)
        end

        if cmp_pt(left, pivot_idx, points, points) then
          left := left + 1
        end

        if not cmp_pt(right, pivot_idx, points, points) then
          right := right - 1
        end
      end
      Result := pivot_idx
    end

end
