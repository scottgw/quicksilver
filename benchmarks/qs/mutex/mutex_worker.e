import Prelude
import Data

class Mutex_Worker

  data: separate Data

  make(a_data: separate Data)
    do
      data := a_data
    end

  run()
    local
      i: Integer
    do
      from i := 1
      until i > 20000 
      loop
        separate data
          do
            data.incr()
          end
        i := i + 1
      end

      {Prelude}.print("Worker done %N")
    end
end
