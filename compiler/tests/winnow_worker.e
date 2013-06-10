import Prelude
import Int_Matrix
import Winnow_Value_Point
import Array

class Winnow_Worker

create make

  start: Integer
  final: Integer

  ncols: Integer

  mask: Int_Matrix
  local_mat: Int_Matrix
  val_points: Array[Winnow_Value_Point]

  sep_mask: separate Int_Matrix
  sep_matrix: separate Int_Matrix
  
  make(a_start: Integer; a_final: Integer; a_ncols: Integer
       a_sep_mask: separate Int_Matrix; a_sep_matrix: separate Int_Matrix)
    do
      start := a_start
      final := a_final
      ncols := a_ncols

      create mask.make_with_start_row (ncols, final - start, start)
      create local_mat.make_with_start_row (ncols, final - start, start)

      sep_mask := a_sep_mask
      sep_matrix := a_sep_matrix

      fetch_mask()
      fetch_matrix()
    end

  gather_unmasked()
    local
      i: Integer
      j: Integer
      count: Integer
      value_point: Winnow_Value_Point
      val_pt_idx: Integer
    do
      count := 0

      from i := start
      until i >= final
      loop
        from j := 0
        until j >= ncols
        loop
          count := count + mask.item(j, i)
          j := j + 1
        end
        i := i + 1
      end

      create val_points.make(count)
      val_pt_idx := 0

      from i := start
      until i >= final
      loop
        from j := 0
        until j >= ncols
        loop
          if mask.item(j, i) = 1 then
            create value_point.make(local_mat.item(j, i), j, i)
            val_points.put (val_pt_idx, value_point)
            val_pt_idx := val_pt_idx + 1            
          end
          j := j + 1
        end
        i := i + 1
      end      
    end


  -- Util
  fetch_matrix()
    local
      i: Integer
      j: Integer
    do
      separate sep_matrix
        do
          from i := start
          until i >= final
          loop
            from j := 0
            until j >= ncols
            loop
              local_mat.put(j, i, sep_matrix.item(j, i))
              j := j + 1
            end
            i := i + 1
          end
        end
      end

  fetch_mask()
    local
      i: Integer
      j: Integer
    do
      separate sep_mask
        do
          from i := start
          until i >= final
          loop
            from j := 0
            until j >= ncols
            loop
              mask.put(j, i, sep_mask.item(j, i))
              j := j + 1
            end
            i := i + 1
          end
        end
    end

end
