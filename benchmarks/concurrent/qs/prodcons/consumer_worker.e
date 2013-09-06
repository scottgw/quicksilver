import Prelude
import Int_Stack

class Consumer_Worker

  stack: separate Int_Stack
  done: Boolean
  
  make(a_stack: separate Int_Stack)
    do
      stack := a_stack
      done := False
    end

  run()
    local
      i: Integer
    do
      from i := 1
      until i > 20000 
      loop
        separate stack
          require
            stack.count > 0
          do
            stack.pop()
          end
        i := i + 1
      end
      done := True
      {Prelude}.print("Worker done %N")
    end
end
