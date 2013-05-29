import Prelude
import Int_Array

class Share_Worker

  array: separate Int_Array

  make(a_array: separate Int_Array)
    do
      array := a_array
    end

  run()
    local
      i: Integer
      v: Integer
    do
      from i := 1
      until i > 20000 
      loop
        separate array
          do
            v := array.item(i)
            array.put(i, v + 1)
          end
        i := i + 1
      end

      {Prelude}.print("Worker done %N")
    end
end
