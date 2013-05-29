import Pointer_64
import Pointer

class Int_Array

create make

  ptr: Pointer_64

  make(n: Integer)
    do
      ptr := {Pointer}.new_pointer_64(n)
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
