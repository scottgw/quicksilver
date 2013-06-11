import Token
import Prelude

class Threadring_Worker

create make

  has_token: Boolean
  token: separate Token
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


  take_token(): separate Token
    do
      has_token := False
      Result := token
    end
  
  pass (a_token: separate Token)
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
          has_token := True
          separate token do token.incr(id) end
          next.run()
        end
      end
    end
  
end


