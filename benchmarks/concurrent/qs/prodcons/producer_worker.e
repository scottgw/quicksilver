import Prelude
import Int_Stack

class Producer_Worker

  stack: separate Int_Stack

  make(a_stack: separate Int_Stack)
    do
      stack := a_stack
    end

  run()
    local
      i: Integer
    do
      from i := 1
      until i > 20000 
      loop
        separate stack
          do
            stack.push(i)
          end
        i := i + 1
      end

      {Prelude}.print("Worker done %N")
    end
end
