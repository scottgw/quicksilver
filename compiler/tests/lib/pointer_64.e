import Pointer

class Pointer_64
  put(i: Integer; v: Integer)
    do
      {Pointer}.pointer_64_put (Current, i, v)
    end

  put_real(i: Integer; v: Real)
    do
      {Pointer}.pointer_real_put (Current, i, v)
    end


  item (i: Integer): Integer
    do
      Result := {Pointer}.pointer_64_get (Current, i)
    end

  item_real (i: Integer): Real
    do
      Result := {Pointer}.pointer_real_get (Current, i)
    end
end
