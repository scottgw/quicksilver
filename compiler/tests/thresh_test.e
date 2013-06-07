import Int_Array
import Int_Matrix
import Thresh_Histogram

module Thresh_Test

  main()
    local
      mat: separate Int_Matrix
      hist: separate Thresh_Histogram
    do
      create mat.make(10, 10)
      create hist.make(0, 10, 10, mat) 
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
