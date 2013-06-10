import Array_Inner

class Array [G]

create make

  count: Integer
  data: Array_Inner [G]
  
  make(n: Integer)
    do
      create data.make (n)
      count := n
    end

  item(i: Integer): G
    do
      Result := data.item (i)
    end

  put(i: Integer; v: G)
    do
      data.put(i, v)
    end

end
