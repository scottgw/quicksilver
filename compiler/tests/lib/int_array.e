import Pointer_64
import Pointer

class Int_Array

create make

  ptr: Pointer_64
  count: Integer

  make(n: Integer)
    do
      ptr := {Pointer}.new_pointer_64(n)
      count := n
    end

  item(i: Integer): Integer
    do
      Result := ptr.item(i)
    end

  put(i: Integer; v: Integer)
    do
      ptr.put(i, v)
    end
end
