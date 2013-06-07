import Pointer_64
import Pointer

class Real_Array

create make

  ptr: Pointer_64

  make(n: Integer)
    do
      ptr := {Pointer}.new_pointer_64(n)
    end

  item(i: Integer): Real
    do
      Result := ptr.item_real(i)
    end

  put(i: Integer; v: Real)
    do
      ptr.put_real(i, v)
    end
end
