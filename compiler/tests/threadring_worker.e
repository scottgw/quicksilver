import Token
import Prelude

class Threadring_Worker

create make

  next: separate Threadring_Worker
  n: Integer
  id: Integer

  make (a_id: Integer; a_next: separate Threadring_Worker; a_n: Integer)
    do
      id := a_id
      n := a_n
      set_next (a_next)
    end

  set_next (a_next: separate Threadring_Worker)
    do
      next := a_next
    end

  pass (a_token: separate Token)
    local
      keep_going: Boolean
    do
      separate a_token
        do
          -- {Prelude}.print({Prelude}.int_to_str (id))
          -- {Prelude}.print(" saw token number: ")
          -- {Prelude}.print({Prelude}.int_to_str (a_token.count))
          -- {Prelude}.print("%N")
          keep_going := a_token.count < n
          if keep_going then
            a_token.incr()
          end
        end

      if keep_going then 
        separate next
          do
            next.pass (a_token)
          end
      else
        {Prelude}.print ("Done!%N")
        {Prelude}.exit_with (0)
      end
    end
  
end


