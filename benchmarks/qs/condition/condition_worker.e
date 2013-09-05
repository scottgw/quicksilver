import Prelude
import Data

class Condition_Worker

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
      l_sign: Integer
    do
      l_sign := sign
      from i := 1
      until i > 5000 
      loop
        separate data
          require
            data.value \\ 2 = l_sign
          do
            data.incr()
          end
        i := i + 1
      end

      {Prelude}.print("Worker done %N")
    end
end
