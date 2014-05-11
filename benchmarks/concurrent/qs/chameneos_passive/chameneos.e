import Broker
import Prelude
import Signal

class Chameneos

create make

  c: Integer
  broker: separate Broker
  signal: separate Signal
  max: Integer

  make(c_: Integer; broker_: separate Broker; signal_: separate Signal; max_: Integer)
    do
      c := c_
      broker := broker_
      signal := signal_
      max := max_
    end

  compl(c1: Integer; c2: Integer): Integer
    do
      if c1 = 0 then
        if c2 = 0 then
          Result := 0
        elseif c2 = 1 then
          Result := 2
        else
          Result := 1
        end
      elseif c1 = 1 then
        if c2 = 0 then
          Result := 2
        elseif c2 = 1 then
          Result := 1
        else
          Result := 0
        end
      else
        if c2 = 0 then
          Result := 1
        elseif c2 = 1 then
          Result := 0
        else
          Result := 2
        end
      end
    end

  request_meeting()
    local
      met: Boolean
      other_cham: separate Chameneos
      other_c: Integer
      n: Integer
    do
      from met := True
      until not met
      loop
        separate broker
          do
            passive broker
              do
                met := broker.register_cham(c, Current)
                if met then
                  other_cham := broker.current_cham
                  other_c := broker.current_c
                  n := broker.n
                  broker.clear()
                end
              end
          end
        if met then
          separate other_cham
            do
              other_cham.meet_with(c, n, True)
            end
           meet_with (other_c, n, False)
        end
      end
    end

  meet_with(other_c: Integer; n: Integer; restart: Boolean)
    do
      c := compl(c, other_c)
      if n >= max then
        -- {Prelude}.print("shutting down%N")
        separate signal
          do
            signal.signal()
          end
      elseif restart then
        request_meeting()
      end
    end

end
