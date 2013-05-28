import Prelude
import Data

class Worker

  data: separate Data
  sign: Integer

  make(a_sign: Integer; a_data: separate Data)
    do
      sign := a_sign
      data := a_data
    end

  run()
    local
      i: Integer
    do
      from i := 1
      until i > 200
      loop
--        {Prelude}.print ("Working%N")
        separate data
          require
            data.get_value() \\ 2 = sign
          do
            data.incr()
          end
        i := i + 1
      end

      {Prelude}.print("Worker done %N")
    end
end
