import Int_Stack_Link

class Int_Stack

create make

  head: Int_Stack_Link
  count: Integer

  make()
    do
      head := Void
      count := 0
    end

  push(i: Integer)
    local
      link: Int_Stack_Link
    do
      create link.make(i, head)
      head := link
      count := count + 1
    end

  pop(): Integer
    do
      Result := head.value
      head := head.next
      count := count - 1
    end

  get_count(): Integer
    do
      Result := count
    end

  is_empty(): Boolean
    do
      Result := head = Void
    end
end
