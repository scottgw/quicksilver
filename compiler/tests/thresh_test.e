import Array
import Int_Array
import Int_Matrix
import Thresh_Histogram
import Prelude

module Thresh_Test

  main()
    local
      n: Integer
      rows: Integer
      cols: Integer
      percent: Integer

      i: Integer
      ii: Integer
      jj: Integer
      iend: Integer
      start: Integer
      height: Integer

      result_mat: Int_Matrix
      mat: separate Int_Matrix

      sep_hist: separate Int_Array
      sep_max: separate Int_Array

      thresh: Integer

      hist: separate Thresh_Histogram
      workers: Array [separate Thresh_Histogram]
    do
      n := 32
      rows := 800
      cols := 800
      percent := 1

      create workers.make(n)
      create mat.make (rows, cols)
      create result_mat.make (rows, cols)
      create sep_max.make (1)
      create sep_hist.make (101)

      separate sep_max
        do
          sep_max.put(0, -1)
        end

      from
        start := 0
        i := 0
      until
        i >= n
      loop
        height := (rows - start) // (n - i)
        create hist.make(start, start + height, cols, sep_max, sep_hist, mat)
        workers.put(i, hist)

        separate hist
          do
            hist.calc_histogram()
          end

        start := start + height
        i := i + 1
      end

      from i := 0
      until i >= n
      loop
        hist := workers.item(i)
        separate hist do hist.start end
        i := i + 1
      end

      thresh := calculate_threshold(rows, cols, percent, sep_max, sep_hist)

      from i := 0
      until i >= n
      loop
        hist := workers.item(i)
        separate hist do hist.threshold(thresh) end
        i := i + 1
      end

      from i := 0
      until i >= n
      loop
        hist := workers.item(i)
        separate hist
          do
            from
              start := hist.start
              ii := start
              iend := hist.final
            until ii >= iend
            loop
              from jj := 0
              until jj >= cols
              loop
                result_mat.put (ii, jj, hist.local_mat.item(ii - start, jj))
                jj := jj + 1
              end
              ii := ii + 1
            end
          end
        shutdown hist
        i := i + 1
      end

      shutdown sep_hist
      shutdown sep_max
      shutdown mat
    end

  calculate_threshold (nrows, ncols, percent: Integer;
                       a_sep_max: separate Int_Array;
                       a_sep_hist: separate Int_Array): Integer
    local
      count: Integer
      threshold: Integer
      prefixsum: Integer
      i: Integer
      h: Integer
    do
      separate a_sep_max
        do
          threshold := a_sep_max.item(0)
        end

      count := (nrows * ncols * percent) // 100

      prefixsum := 0

      separate a_sep_hist
        do   
          from i := threshold
          until not(i >= 0 and prefixsum <= count)
          loop
            h := a_sep_hist.item (i)
            prefixsum := prefixsum + h
            threshold := i;
            i := i - 1
          end
        end

      Result := threshold
    end
end