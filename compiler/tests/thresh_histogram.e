import Prelude
import Int_Array
import Int_Matrix

class Thresh_Histogram

create make

  start: Integer
  final: Integer
  ncols: Integer

  source: separate Int_Matrix

  local_mat: Int_Matrix

  -- Histogram
  hist: Int_Array
  max: Integer

  sep_max: separate Int_Array
  sep_hist: separate Int_Array
  
  
  make(a_start: Integer; a_final: Integer;
       a_ncols: Integer
       a_sep_max: separate Int_Array
       a_sep_hist: separate Int_Array
       a_source: separate Int_Matrix)
    do
      start := a_start
      final := a_final
      ncols := a_ncols

      sep_max := a_sep_max
      sep_hist := a_sep_hist

      source := a_source
      create local_mat.make(ncols, final - start)
    end

  -- Histogram calculation
  calc_histogram()
    do
      fetch_matrix()
      calc_result()
      merge_result()
    end

  merge_result()
    local
      i: Integer
      h: Integer
    do
      separate sep_max
        do
          sep_max.put(0, {Prelude}.int_max(sep_max.item(0), max))
        end

      separate sep_hist
        do
          from i := 0
          until i >= 100
          loop
            h := sep_hist.item (i)
            sep_hist.put (i, h + hist.item(i))
            i := i + 1
          end      
        end
    end

  calc_result()
    local
      i, j: Integer
      e: Integer
    do
      create hist.make(101)
      max := 0

      from i := 0
      until i >= 100
      loop
        hist.put(i, 0)
        i := i + 1
      end

      from i := start
      until i >= final
      loop
        from j := 1
        until j >= ncols
        loop
          e        := local_mat.item(i - start, j)
          hist.put(e, hist.item(e) + 1)
          max      := {Prelude}.int_max(e, max)
          j := j + 1
        end
        i := i + 1
      end
    end

  -- Thresholding on local matrix
  threshold(cutoff: Integer)
    local
      i: Integer
      j: Integer
    do
      from i := start
      until i >= final
      loop
        from j := 0
        until j >= ncols
        loop
          if local_mat.item(i - start, j) >= cutoff then
            local_mat.put(i - start, j, 1)
          else
            local_mat.put(i - start, j, 0)
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
      separate source
        do
          from i := start
          until i >= final
          loop
            from j := 0
            until j >= ncols
            loop
              local_mat.put(i - start, j, source.item(i, j))
              j := j + 1
            end
            i := i + 1
          end
        end
    end
end
