import Prelude
import Data

class Condition_Worker

  data: separate Data
  sign: Integer
  done: Boolean
  iters: Integer
  
  make(a_sign: Integer; a_iters: Integer; a_data: separate Data)
    do
      sign := a_sign
      iters := a_iters
      data := a_data
      done := False
    end

  run()
    local
      i: Integer
      l_sign: Integer
      l_iters: Integer
    do
      l_sign := sign
      l_iters := iters
      from i := 1
      until i > l_iters
      loop
        separate data
          require
            data.value \\ 2 = l_sign
          do
            passive data
              do
                data.incr()
              end
          end
        i := i + 1
      end

      separate data
        do
          data.set_done()
        end

      done := True
      {Prelude}.print("Worker done %N")
    end
end
