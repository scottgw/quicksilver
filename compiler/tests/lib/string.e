import Pointer
import Pointer_8
import Prelude

class String
  length: Integer
  data: Pointer_8

  make (n: Integer)
    local
      i: Integer_8
    do
      length := n
      
      data := {Pointer}.new_pointer_8 (n + 1)
      data.put (n, {Prelude}.int8_to_char(0))
    end

  make_with_pointer (n: Integer; p: Pointer_8)
    do
      length := n
      data := p
    end

  item (i: Integer): Character_8
    do
      Result := data.char_item (i - 1)
    end

  starts_with (other: String): Boolean
    local
      i: Integer
      found_different: Boolean
    do
      Result := False

      if other.length <= length then
        found_different := False

        from
          i := 1
        until
          i > other.length or found_different
        loop
          found_different := item(i) /= other.item(i)
          i := i + 1
        end

        Result := not found_different
      end
    end
  
  append (other: String): String
    local
      ptr: Pointer_8
      i: Integer
    do
      create Result.make (length + other.length)

      ptr := Result.data
      
      from i := 1
      until i > length
      loop
        ptr.put (i - 1, item (i))
        i := i + 1
      end

      from
      until i > length + other.length
      loop
        ptr.put (i - 1, other.item (i - length))
        i := i + 1
      end
    end

  substring (start: Integer; finish: Integer): String
    local
      ptr: Pointer_8
      i: Integer
    do
      create Result.make (finish - start)

      ptr := Result.data

      from i := start
      until i >= finish
      loop
        ptr.put(i - start, item(i))
        i := i + 1
      end
    end
end
