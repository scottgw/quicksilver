import Array
import Int_Array
import Chameneos
import Broker
import Prelude

module Main
  
  main()
    local
      chms1: Int_Array
      chms2: Int_Array
      n: Integer
    do
      n := 6000000
      create chms1.make(3)
      chms1.put(0, 0)
      chms1.put(1, 1)
      chms1.put(2, 2)
      run (n, chms1)
      {Prelude}.print("Finished first%N")

      create chms2.make(10)
      chms2.put(0, 0)
      chms2.put(1, 1)
      chms2.put(2, 2)
      chms2.put(3, 1)
      chms2.put(4, 2)
      chms2.put(5, 0)
      chms2.put(6, 1)
      chms2.put(7, 2)
      chms2.put(8, 1)
      chms2.put(9, 0)
      run (n, chms2)
      {Prelude}.print("Finished second%N")
    end

  run(n: Integer; chms: Int_Array)
    local
      signal: separate Signal
      worker: separate Chameneos
      broker: separate Broker
      workers: Array [separate Chameneos]
      i: Integer
    do
      create signal.make()
      create broker.make(n)
      create workers.make(chms.count)

      from i := 0
      until i >= chms.count
      loop
        create worker.make(chms.item(i), broker, signal, n)
        separate worker
          do
            worker.request_meeting()
          end
        workers.put(i, worker)
        i := i + 1
      end

      separate signal
        require signal.done
        do
        end

      from i := 0
      until i >= chms.count
      loop
        shutdown workers.item(i)
        i := i + 1
      end
      shutdown signal
      shutdown broker
    end
end
