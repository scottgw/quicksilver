import Array
import Winnow_Value_Point
import Prelude

module Winnow_Sort

  sort_val_points (points: Array[Winnow_Value_Point]): Array [Winnow_Value_Point]
    do
      {Prelude}.print({Prelude}.int_to_str(points.count))
      {Prelude}.print("%N")
 
      sort_range(points, 0, points.count)
    end

  sort_range(points: Array[Winnow_Value_Point];
             start: Integer; final: Integer
             ): Array [Winnow_Value_Point]
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

  cmp_pt(p1: Winnow_Value_Point;
         p2: Winnow_Value_Point): Boolean
    do
      if p1.value <= p2.value then
        Result := True
      elseif p1.value = p2.value then
        if p1.x <= p2.x then
          Result := True
        elseif p1.x = p2.x then
          Result := p1.y <= p2.y
        else
          Result := False
        end
      else
        Result := False
      end
    end

  partition (points: Array[Winnow_Value_Point];
             start: Integer;
             final: Integer
             ): Integer
    local
      i: Integer
      piv_pt: Winnow_Value_Point
      left_pt: Winnow_Value_Point
      right_pt: Winnow_Value_Point
      left: Integer
      right: Integer
      pivot_idx: Integer
    do
      pivot_idx := start + (final - start) // 2
      piv_pt := points.item(pivot_idx)

      from
        left := start
        right := final - 1
      until left >= right
      loop
        left_pt := points.item (left)
        right_pt := points.item (right)

        if (not cmp_pt(left_pt, piv_pt)) and
           cmp_pt(piv_pt, right_pt) then
          points.put (left, right_pt)
          points.put (right, left_pt)
        end

        if cmp_pt(left_pt, piv_pt) then
          left := left + 1
        end

        if not cmp_pt(right_pt, piv_pt) then
          right := right - 1
        end
      end
      Result := pivot_idx
    end

end
