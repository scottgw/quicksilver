import Int_array
import Array
import Share_Worker

module Share_Test
  
  main()
    local
      i: Integer
      data: separate Int_Array
      workers: Array [separate Share_Worker]
      worker: separate Share_Worker
      n: Integer
    do
      n := 32
      create data.make(20000)
      create workers.make(n)

      from i := 0
      until i >= n
      loop
        create worker.make(data)
        separate worker
          do
            worker.run()
          end
        workers.put(i, worker)
        i := i + 1
      end

      from i := 0
      until i >= n
      loop
        shutdown workers.item(i)
        i := i + 1
      end
      shutdown data
    end
end
