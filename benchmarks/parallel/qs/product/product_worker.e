import Prelude
import Real_Matrix
import Real_Array
import Real_Math

class Product_Worker

create make

  start: Integer
  final: Integer

  nelts: Integer
  
  matrix: Real_Matrix
  vector: Real_Array

  sep_matrix: separate Real_Matrix
  sep_vector: separate Real_Array

  prod_vector: Real_Array

  time: Real

  make(a_start: Integer; a_final: Integer; a_nelts: Integer;
       a_sep_matrix: separate Real_Matrix;
       a_sep_vector: separate Real_Array)
    do
      start := a_start
      final := a_final
      nelts := a_nelts

      sep_matrix := a_sep_matrix
      sep_vector := a_sep_vector

      create prod_vector.make (final - start)
      create matrix.make (final - start, nelts)
      create vector.make (nelts)
    end

  calculate()
    do
      fetch_points()
      calc_product()
    end

  fetch_points()
    local
      i: Integer
      j: Integer
      s: Integer
      f: Integer
      n: Integer
      m: Real_Matrix
      sm: separate Real_Matrix
    do
      n := nelts
      s := start
      f := final
      m := matrix
      sm := sep_matrix

      separate sep_vector
        do
			  from
				  i := 0
				  sep_vector.count
          until i >= n
          loop
            vector.put (i, sep_vector.item (i))
            i := i + 1
          end
        end

      separate sm
        do
			  from
				  i := s
				  sm.height
          until i >= f
          loop
            from j := 0
            until j >= n
            loop
              m.put (j, i - s, sm.item (j, i))
              j := j + 1
            end
            i := i + 1
          end
        end
    end

  calc_product()
    local
      sum: Real
      i, j: Integer
      m: Real_Matrix
      v: Real_Array
      p: Real_Array
    do
      m := matrix
      v := vector
      p := prod_vector
      time := {Prelude}.get_time()

      from i := start
      until i >= final
      loop
        sum := 0.0
        from j := 0
        until j >= nelts
        loop
          sum := sum + v.item(j) * m.item(j, i - start)
          j := j + 1
        end
        p.put (i - start, sum)

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
