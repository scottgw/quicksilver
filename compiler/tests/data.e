class Data

create make

  value: Integer

  make()
    do
      value := 0
    end

  incr()
    do
      value := value + 1
    end

  set_value(a_value: Integer)
    do
      value := a_value
    end
end
