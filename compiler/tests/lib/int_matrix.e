import Int_Array

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
end
