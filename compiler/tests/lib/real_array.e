import Pointer_64
import Pointer

class Real_Array

create make, make_with_base

  ptr: Pointer_64
  count: Integer
  start: Integer

  
  make_with_base (n: Integer; a_start: Integer)
    do
      ptr := {Pointer}.new_pointer_64(n)
      start := a_start
      n := count
    end
  
  make(n: Integer)
    do
      make_with_base (n, 0)
    end

  item(i: Integer): Real
    do
      Result := ptr.item_real(i - start)
    end

  put(i: Integer; v: Real)
    do
      ptr.put_real(i - start, v)
    end
end
