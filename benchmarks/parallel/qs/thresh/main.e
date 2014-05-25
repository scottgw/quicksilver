import Array
import Int_Array
import Int_Matrix
import Prelude
import Real_Math
import Thresh_Histogram

module Main

  main()
    local
      n: Integer
      nelts: Integer
      percent: Integer

      i: Integer
      ii: Integer
      jj: Integer
      iend: Integer
      start: Integer
      height: Integer

      result_mask: Int_Matrix
      mat: separate Int_Matrix

      sep_hist: separate Int_Array
      sep_max: separate Int_Array

      thresh: Integer

      worker: separate Thresh_Histogram
      worker_mask: separate Int_Matrix
      workers: Array [separate Thresh_Histogram]
      worker_time: Real

      time: Real
    do
      n := {Prelude}.get_int_env("LIBQS_EXECS")
      nelts := 10000
      percent := 1

      create workers.make(n)
      create mat.make (nelts, nelts)
      create result_mask.make (nelts, nelts)
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
        height := (nelts - start) // (n - i)
        create worker.make(start, start + height, nelts, sep_max, sep_hist, mat)
        workers.put(i, worker)

        separate worker
          do
            worker.calc_histogram()
          end

        start := start + height
        i := i + 1
      end

      from i := 0
      until i >= n
      loop
        worker := workers.item(i)
        separate worker do worker.start end
        i := i + 1
      end

      time := {Prelude}.get_time()
      thresh := calculate_threshold(nelts, percent, sep_max, sep_hist)
      time := {Prelude}.get_time() - time
      from i := 0
      until i >= n
      loop
        worker := workers.item(i)
        separate worker do worker.threshold(thresh) end
        i := i + 1
      end

      worker_time := 0.0
      from i := 0
      until i >= n
      loop
        worker := workers.item(i)
        worker_time := fetch_one (worker, worker_time, nelts, result_mask)
        shutdown worker
        i := i + 1
      end
      time := time + worker_time
      {Prelude}.print_err({Prelude}.real_to_str(time))
      {Prelude}.print_err("%N")
      {Prelude}.exit_with(0)

--      shutdown sep_hist
--      shutdown sep_max
--      shutdown mat
    end

  fetch_one (worker: separate Thresh_Histogram;
             worker_time: Real;
             nelts: Integer;
             result_mask: Int_Matrix): Real
    local
      start, iend, ii, jj: Integer
      worker_mask: separate Int_Matrix
    do
      separate worker
        do
          worker_mask := worker.mask
          start := worker.start
          iend := worker.final
          Result := {Real_Math}.max(worker.time, worker_time)
        end

      separate worker_mask
        do
          from
             ii := start
				 worker_mask.height
          until ii >= iend
          loop
            from jj := 0
            until jj >= nelts
            loop
              result_mask.put (jj, ii, worker_mask.item(jj, ii - start))
              jj := jj + 1
            end
            ii := ii + 1
          end
        end
    end 

  calculate_threshold (nelts, percent: Integer;
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

      count := (nelts * nelts * percent) // 100

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
