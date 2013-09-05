import Token
import Prelude

class Threadring_Worker

create make

  has_token: Boolean
  token: Integer
  next: separate Threadring_Worker
  id: Integer

  make (a_id: Integer; a_next: separate Threadring_Worker)
    do
      has_token := False
      id := a_id
      set_next (a_next)
    end

  set_next (a_next: separate Threadring_Worker)
    do
      next := a_next
    end


  take_token(): Integer
    do
      has_token := False
      Result := token
    end
  
  pass (a_token: Integer)
    do
      token := a_token
      has_token := True
    end

  
  run()
    do
      separate next
        require
          next.has_token
        do
          token := next.take_token()
          if token = 0 then
            {Prelude}.print({Prelude}.int_to_str(id))
            {Prelude}.print("%N")
            {Prelude}.exit_with (0)
          end
          token := token - 1
          has_token := True
          next.run()
        end
    end
  
end


