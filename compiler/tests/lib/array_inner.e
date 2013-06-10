class Array_Inner [G]

create make

  make(n: Integer)
    external "array_make"
    end

  item(i: Integer): G
    external "array_item"
    end

  put(i: Integer; v: G)
    external "array_put"
    end
end
