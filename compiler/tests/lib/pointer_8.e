import Pointer

class Pointer_8
  put(i: Integer; v: Character_8)
    do
      {Pointer}.pointer_8_put (Current, i, v)
    end

  char_item (i: Integer): Character_8
    do
      Result := {Pointer}.pointer_8_get (Current, i)
    end
end
