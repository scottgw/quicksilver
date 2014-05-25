import Array
import Int_Array
import Prelude
import Real_Matrix
import Real_Array
import Real_Math

class Outer_Worker

create make

  start: Integer
  final: Integer

  nelts: Integer
  
  matrix: Real_Matrix
  vector: Real_Array

  sep_x_points: separate Int_Array
  sep_y_points: separate Int_Array
  
  x_points: Int_Array
  y_points: Int_Array

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

      create x_points.make (nelts)
      create y_points.make (nelts)

      create matrix.make (final - start, nelts)
      create vector.make (final - start)
    end

  calculate()
    do
      fetch_points()
      calc_outer()
    end

  fetch_points()
    local
      i: Integer
    do
      separate sep_x_points
        do
			  from
				  i := 0
				  sep_x_points.count
          until i >= nelts
          loop
            x_points.put (i, sep_x_points.item(i))
            i := i + 1
          end
        end

      separate sep_y_points
        do
			  from
				  i := 0
				  sep_y_points.count
          until i >= nelts
          loop
            y_points.put (i, sep_y_points.item(i))
            i := i + 1
          end
        end
    end

  calc_outer()
    local
      nmax: Real
      d: Real
      p1x, p2x, p1y, p2y: Integer
      i, j: Integer
    do
      time := {Prelude}.get_time()

      from i := start
      until i >= final
      loop
        nmax := -1.0

        p1x := x_points.item(i - start)
        p1y := y_points.item(i - start) 

        from j := 0
        until j >= nelts
        loop
          if i /= j then
            p2y := y_points.item(j)
            p2x := x_points.item(j)
            d := distance (p1x, p1y, p2x, p2y)
            matrix.put (j, i - start, d)
            nmax := {Real_Math}.max (nmax, d)
          end

          j := j + 1
        end

        matrix.put (i, i - start, nmax * {Real_Math}.from_int (nelts))
        vector.put (i - start, distance (0, 0, p1x, p1y))

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
