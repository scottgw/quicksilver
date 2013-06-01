import Prelude
import Data

class Noshare_Worker

create make
  make()
    do
    end

  fib (i: Integer): Integer
    do
      if i < 2 then
        Result := 1
      else
        Result := fib (i - 1) + fib (i - 2)
      end
    end

  run()
    do
      {Prelude}.print({Prelude}.int_to_str(fib(40)).append("%N"))
    end
end
