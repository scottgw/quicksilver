import Array
import Prelude
import Real_Matrix
import Real_Array
import Real_Math
import Winnow_Point

class Outer_Worker

create make

  start: Integer
  final: Integer

  nelts: Integer
  
  matrix: Real_Matrix
  vector: Real_Array

  sep_points: separate Array [Winnow_Point]
  points: Array [Winnow_Point]

  make(a_start: Integer; a_final: Integer; a_nelts: Integer
      ;a_sep_points: separate Array [Winnow_Point])
    do
      start := a_start
      final := a_final
      nelts := a_nelts

      sep_points := a_sep_points
      
      create matrix.make_with_start_row (nelts, final - start, nelts)
      create vector.make_with_base (final - start, start)
    end

  calculate()
    do
      fetch_points()
      calc_outer(points)
    end

  fetch_points()
    local
      i: Integer
      pt: Winnow_Point
    do
      separate sep_points
        do
          from i := start
          until i >= final
          loop
            create pt.make (sep_points.item(i).x, sep_points.item(i).y)
            points.put (i, pt)
            i := i + 1
          end
        end
    end

  calc_outer(a_points: Array [Winnow_Point])
    local
      nmax: Real
      d: Real
      p1, p2: Winnow_Point
      i, j: Integer
    do
      from i := start
      until i >= final
      loop
        nmax := -1.0
        p1 := a_points.item(i)
        from j := 0
        until j >= nelts
        loop
          if i /= j then
            p2 := a_points.item(j)
            d := distance (p1.x, p1.y, p2.x, p2.y)
            matrix.put (j, i, d)
            nmax := {Real_Math}.max (nmax, d)
          end
          j := j + 1
        end
        matrix.put (i, i, nmax * {Real_Math}.from_int (nelts))
        vector.put (i, distance (0, 0, a_points.item(i).x, a_points.item(i).y))
        i := i + 1
      end
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
  
end
