import Array
import Chain_Worker
import Int_Array
import Prelude
import Real_Array
import Winnow_Gatherer

module Main

  main()
    local
      nelts: Integer
      s: Integer
      percent: Integer
      winnow_nelts: Integer

      num_workers: Integer
      workers: Array[separate Chain_Worker]

      height: Integer
      start: Integer
      i, j: Integer
      worker: separate Chain_Worker

      -- Results of thresh stage
      sep_max: separate Int_Array
      sep_hist: separate Int_Array
      thresh: Integer
      
      -- Result of winnow stage:
      winnow_gatherer: separate Winnow_Gatherer

      -- Results of outer stage:
      shared_outer_vector: separate Real_Array
      shared_outer_count: separate Int_Array

      -- Result of product stage
      result_vector: Real_Array
    do
      nelts := 10000
      s := 0
      percent := 1
      winnow_nelts := 10000

      num_workers := {Prelude}.get_int_env("LIBQS_EXECS")
      create workers.make (num_workers)

      {Prelude}.print("Master: starting randmat%N")
      from
        start := 0
        i := 0
      until i >= workers.count
      loop
        height := (nelts - start) // (num_workers - i)

        create worker.make (start, height, nelts, s, winnow_nelts)
        separate worker
          do
            worker.start_randmat()
          end

        workers.put(i, worker)

        start := start + height
        i := i + 1
      end

      -- Begin thresh, calculate threshold
      {Prelude}.print("Master: starting thresh%N")
      create sep_max.make(1)
      separate sep_max
        do
          sep_max.put(0, 0)
        end

      create sep_hist.make(100)
      separate sep_hist
        do
          from
            i := 0
          until
            i >= 100
          loop
            sep_hist.put(i, 0)
            i := i + 1
          end
        end

      from
        i := 0
      until
        i >= workers.count
      loop
        worker := workers.item(i)
        separate worker
          do
            worker.start_thresh(sep_max, sep_hist)
          end
        i := i + 1
      end
      {Prelude}.print("Master: waiting to finish thresh%N")
      -- Wait until all workers are done with their thresholds
      from
        i := 0
      until
        i >= workers.count
      loop
        worker := workers.item(i)
        separate worker
          do
            worker.start
          end
        i := i + 1
      end

      {Prelude}.print("Master: calculating thresh%N")
      thresh := calc_threshold(nelts, percent, sep_max, sep_hist)
      {Prelude}.print("Master: resuming thresh%N")

      from
        i := 0
      until
        i >= workers.count
      loop
        worker := workers.item(i)
        separate worker
          do
            worker.resume_thresh(thresh)
          end
        i := i + 1
      end

      -- For winnow
      {Prelude}.print("Master: starting winnow gather%N")
      create winnow_gatherer.make(winnow_nelts)
      fetch_winnow(workers, winnow_gatherer)

      -- For outer
      {Prelude}.print("Master: starting outer%N")
      create shared_outer_vector.make(winnow_nelts)
      create shared_outer_count.make(1)
      separate shared_outer_count
        do
          shared_outer_count.put(0, 0)
        end
      from
        start := 0
        i := 0
      until i >= workers.count
      loop
        height := (winnow_nelts - start) // (num_workers - i)
        worker := workers.item(i)
        separate worker
          do
            worker.start_outer(start, height,
                               winnow_gatherer,
                               shared_outer_vector,
                               shared_outer_count)
          end

        start := start + height
        i := i + 1
      end      

      -- For product
      create result_vector.make(winnow_nelts)
      {Prelude}.print("Master: starting product fetch%N")
      fetch_product(workers, result_vector)
      {Prelude}.print("Master: exit%N")
      {Prelude}.exit_with(0)
    end

  calc_threshold (nelts, percent: Integer;
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

    -- Set the gatherer to receive data from each worker.
  fetch_winnow(workers: Array[separate Chain_Worker];
               winnow_gatherer: separate Winnow_Gatherer)
    local
      i: Integer
      worker: separate Chain_Worker
    do
      from
        i := 0
      until
        i >= workers.count
      loop
        worker := workers.item (i)
        separate winnow_gatherer
          do
            winnow_gatherer.fetch(worker)
          end
        i := i + 1
      end

      separate winnow_gatherer
        require winnow_gatherer.num_merged = workers.count
          do
            {Prelude}.print("Winnow_Gatherer: done, chunking%N")
            winnow_gatherer.chunk()
          end
    end

  fetch_product(workers: Array[separate Chain_Worker];
                vector: Real_Array)
    local
      i: Integer
      j: Integer
      start: Integer
      final: Integer
      worker: separate Chain_Worker
      worker_vector: separate Real_Array
    do
      from
        i := 0
      until
        i >= workers.count
      loop
        worker := workers.item(i)
        separate worker
          do
            worker_vector := worker.product
            start := worker.start
            final := worker.final
          end

        separate worker_vector
          do
            {Prelude}.print("Master: fetching product%N")
            from
              j := start
            until
              j >= final
            loop
              vector.put(j, worker_vector.item(j - start))
              j := j + 1
            end
          end
        i := i + 1
      end
    end
end    
