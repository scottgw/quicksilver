import Data
import Prelude

class Incr

create make

  sign: Integer
  data: separate Data

  make(a_sign: Integer; a_data: separate Data)
    do
      sign := a_sign
      data := a_data
    end

  run()
    local
      i: Integer
      v: Integer
    do
      from i := 0
      until i > 10
      loop
        separate data
          require data.get_value() \\ 2 = sign
          do
            v := data.get_value()
            {Prelude}.print(("Incr of sign ").append
                            ({Prelude}.int_to_str(sign)).append(" "))
            {Prelude}.print({Prelude}.int_to_str(v).append("%N"))
            data.incr()
          end
        i := i + 1
      end

      shutdown data
    end

end
