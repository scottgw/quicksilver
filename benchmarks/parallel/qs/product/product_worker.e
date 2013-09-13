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
  
  make(a_start: Integer; a_final: Integer; a_nelts: Integer;
       a_sep_matrix: separate Real_Matrix;
       a_sep_vector: separate Real_Array)
    do
      start := a_start
      final := a_final
      nelts := a_nelts

      sep_matrix := a_sep_matrix
      sep_vector := a_sep_vector

      create prod_vector.make_with_base (final - start, start)
      create matrix.make_with_start_row (nelts, final - start, start)
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
    do
      separate sep_vector
        do
          from i := 0
          until i >= nelts
          loop
            vector.put (i, sep_vector.item (i))
            i := i + 1
          end
        end

      separate sep_matrix
        do
          from i := start
          until i >= final
          loop
            from j := 0
            until j >= nelts
            loop
              matrix.put (j, i, sep_matrix.item (j, i))
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
    do
      from i := start
      until i >= final
      loop
        sum := 0.0
        from j := 0
        until j >= nelts
        loop
          sum := sum + vector.item(j) * matrix.item(j, i)
          j := j + 1
        end
        prod_vector.put (i, sum)

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
