import Real_Array

class Real_Matrix

create make

  height: Integer
  width: Integer

  array: Real_Array
  
  make(h: Integer; w: Integer)
    do
      height := h
      width := w
      create array.make(w * h)
    end

  item(x: Integer; y: Integer): Real
    do
      Result := array.item(x + y*width)
    end

  put(x: Integer; y: Integer; v: Real)
    do
      array.put(x + y*width, v)
    end
end
