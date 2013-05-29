import Int_Array

class Int_Matrix

create make

  height: Integer
  width: Integer

  array: Int_Array
  
  make(h: Integer; w: Integer)
    do
      height := h
      width := w
      create array.make(w * h)
    end

  item(x: Integer; y: Integer): Integer
    do
      Result := array.item(x + y*width)
    end

  put(x: Integer; y: Integer; v: Integer)
    do
      array.put(x + y*width, v)
    end
end
