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
      s: Integer
    do
      s := sign
      from i := 1
      until i > 5000 
      loop
        separate data
          require
            data.value \\ 2 = s
          do
            data.incr()
          end
        i := i + 1
      end

      {Prelude}.print("Worker done %N")
    end
end
