import Pointer
import Pointer_8

class String
  length: Integer
  data: Pointer_8

  make (n: Integer)
    do
      length := n
      data := {Pointer}.new_pointer_8 (n + 1)
      data.put (n, 0)
    end

  make_with_pointer (n: Integer; p: Pointer_8)
    do
      length := n
      data := p
    end

  item (i: Integer): Character_8
    do
      Result := data.char_item (i)
    end

  append (str: String): String
    do
      
    end
end
