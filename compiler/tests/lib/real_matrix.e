import Real_Array

class Real_Matrix

create make, make_with_start_row

  height: Integer
  width: Integer

  start_row: Integer
  
  array: Real_Array

  make (h: Integer; w: Integer)
    do
      make_with_start_row (h, w, 0)
    end
  
  make_with_start_row (h: Integer; w: Integer; a_start_row: Integer)
    do
      height := h
      width := w
      start_row := a_start_row
      create array.make(w * h)
    end

  item(x: Integer; y: Integer): Real
    do
      Result := array.item(calc_index (x, y))
    end

  put(x: Integer; y: Integer; v: Real)
    do
      array.put(calc_index(x, y), v)
    end

  calc_index(x: Integer; y: Integer): Integer
    do
      Result : = x + (y - start_row) * width
    end
end
