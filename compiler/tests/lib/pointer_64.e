import Pointer

class Pointer_64
  put(i: Integer; v: Integer)
    do
      {Pointer}.pointer_64_put (Current, i, v)
    end

  item (i: Integer): Integer
    do
      Result := {Pointer}.pointer_64_get (Current, i)
    end
end
