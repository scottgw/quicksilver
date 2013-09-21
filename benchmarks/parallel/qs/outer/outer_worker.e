import Array
import Int_Array
import Prelude
import Real_Matrix
import Real_Array
import Real_Math
import Outer_Point

class Outer_Worker

create make

  start: Integer
  final: Integer

  nelts: Integer
  
  matrix: Real_Matrix
  vector: Real_Array

  sep_x_points: separate Int_Array
  sep_y_points: separate Int_Array
  points: Array [Outer_Point]

  time: Real

  make(a_start: Integer; a_final: Integer; a_nelts: Integer;
       a_sep_x_points: separate Int_Array;
       a_sep_y_points: separate Int_Array)
    do
      start := a_start
      final := a_final
      nelts := a_nelts

      sep_x_points := a_sep_x_points
      sep_y_points := a_sep_y_points

      create points.make (nelts)
      create matrix.make (nelts, final - start)
      create vector.make (final - start)
    end

  calculate()
    do
      fetch_points()
      calc_outer(points)
    end

  fetch_points()
    local
      i: Integer
      pt: Outer_Point
    do
      separate sep_x_points sep_y_points
        do
          from i := 0
          until i >= nelts
          loop
            create pt.make (sep_x_points.item(i), sep_y_points.item(i))
            points.put (i, pt)
            i := i + 1
          end
        end
    end

  calc_outer(a_points: Array [Outer_Point])
    local
      nmax: Real
      d: Real
      p1, p2: Outer_Point
      i, j: Integer
    do
      time := {Prelude}.get_time()
      from i := start
      until i >= final
      loop
        nmax := -1.0
        p1 := a_points.item(i - start)
        from j := 0
        until j >= nelts
        loop
          if i /= j then
            p2 := a_points.item(j)
            d := distance (p1.x, p1.y, p2.x, p2.y)
            matrix.put (j, i - start, d)
            nmax := {Real_Math}.max (nmax, d)
          end
          j := j + 1
        end
        matrix.put (i, i - start, nmax * {Real_Math}.from_int (nelts))
        vector.put (i, distance (0, 0, p1.x, p2.y))
        i := i + 1
      end
      time := {Prelude}.get_time() - time
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
