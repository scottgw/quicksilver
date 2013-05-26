class Data

create make

  value: Integer

  make()
    do
      value := 0
    end

  get_value(): Integer
    do
      Result := value
    end

  incr()
    do
      value := value + 1
    end
end
