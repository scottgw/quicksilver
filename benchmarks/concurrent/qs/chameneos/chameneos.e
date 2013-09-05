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
    do
      separate broker
        do
          broker.register_cham(c, Current)
        end
    end

  meet_with(other_c: Integer; n: Integer)
    do
      c := compl(c, other_c)
      if n >= max then
        separate signal
          do
            signal.signal()
          end
      else
        request_meeting()
      end
    end

end
