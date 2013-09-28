import Int_Array
import String
import Prelude

class Int_Matrix

create make

  height: Integer
  width: Integer

  array: Int_Array

  make (h: Integer; w: Integer)
    do
      height := h
      width := w

      create array.make(w * h)
    end
  
  item (x: Integer; y: Integer): Integer
    do
      Result := array.item(calc_index(x,y))
    end

  put (x: Integer; y: Integer; v: Integer)
    do
      array.put(calc_index(x,y), v)
    end

  calc_index(x: Integer; y: Integer): Integer
    do
      Result : = x + y * width
    end

  to_string(): String
    local
      i, j: Integer
      acc: String
    do
      acc := ""
      from
        i := 0
      until
        i >= height
      loop
        from
          j := 0
        until
          j >= width
        loop
          acc := acc.append ({Prelude}.int_to_str (item (j, i)))
          acc := acc.append_char(' ')
          j := j + 1
        end
        acc := acc.append_char ('%N')
        i := i + 1
      end
      Result := acc
    end
end
