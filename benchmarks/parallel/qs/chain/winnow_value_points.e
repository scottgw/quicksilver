import Int_Array

class Winnow_Value_Points

create make

  data: Int_Array 
  count: Integer

  make(a_count: Integer)
    do
      count := a_count
      create data.make(3 * a_count)
    end

  swap (i, j: Integer)
    local
      v, x, y: Integer 
    do
      v := item_v (i)
      x := item_x (i)
      y := item_y (i)
      put (i, item_v (j), item_x (j), item_y (j))
      put (j, v, x, y)
    end

  put (i: Integer; v, x, y: Integer)
    do
      put_v (i, v)
      put_x (i, x)
      put_y (i, y)
    end

  put_from_other (i: Integer; other: Winnow_Value_Points; j: Integer)
    do
      put (i, other.item_v (j), other.item_x (j), other.item_y (j))
    end

  put_v (i: Integer; v: Integer)
    do
      data.put(3 * i, v)
    end

  put_x (i: Integer; x: Integer)
    do
      data.put(3 * i + 1, x)
    end

  put_y (i: Integer; y: Integer)
    do
      data.put(3 * i + 2, y)
    end

  item_v (i: Integer): Integer
    do
      Result := data.item(3 * i)
    end

  item_x (i: Integer): Integer
    do
      Result := data.item(3 * i + 1)
    end

  item_y (i: Integer): Integer
    do
      Result := data.item(3 * i + 2)
    end

end
