class Signal

create make

  done: Boolean

  make()
    do
      done := False
    end

  signal()
    do
      done := True
    end
end
