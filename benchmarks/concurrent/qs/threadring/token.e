import Threadring_Worker
import Prelude

class Token

create make

  count: Integer
  n: Integer

  make(a_n: Integer)
    do
      n := a_n
      count := 0
    end

  incr(a_id: Integer)
    do
      if count < n then
        -- {Prelude}.print({Prelude}.int_to_str(count))
        -- {Prelude}.print("%N")
        count := count + 1
      else
        {Prelude}.print({Prelude}.int_to_str(a_id))
        {Prelude}.print("%N")
        {Prelude}.exit_with (0)
      end
    end
end
