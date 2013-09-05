import Chameneos
import Prelude

class Broker

create make

  n: Integer
  current_cham: separate Chameneos
  current_c: Integer

  make()
    do
      current_cham := Void
      n := 0
    end
  
  register_cham(c: Integer; cham: separate Chameneos)
    do
      if n \\ 100 = 0 then
        -- {Prelude}.print("Broker working%N")
      end
      if current_cham = Void then
        current_cham := cham
        current_c := c
      else
        send_cham(current_c, cham)
        send_cham(c, current_cham)
        current_cham := Void
        n := n + 1
      end
    end

  send_cham(c: Integer; cham: separate Chameneos) 
   do
      separate cham
        do
          cham.meet_with(c, n)
        end
      end

end

