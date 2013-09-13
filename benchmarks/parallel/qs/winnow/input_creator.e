import Int_Matrix

class Input_Creator

create make

  make(n: Integer)
    do
      load_matrix(n, n)
      load_mask(n, n)
    end
  
  load_matrix (cols: Integer; rows: Integer)
    local
      i: Integer
      j: Integer
    do
      create matrix.make (cols, rows)

      from i := 0
      until i >= rows
      loop
        from j := 0
        until j >= cols
        loop
          matrix.put (j, i, i*j)
          j := j + 1
        end
        i := i + 1
      end
    end

  load_mask (cols: Integer; rows: Integer)
    local
      i: Integer
      j: Integer
    do
      create mask.make (cols, rows)

      from i := 0
      until i >= rows
      loop
        from j := 0
        until j >= cols
        loop
          if ((i * j) \\ (cols + 1)) = 1 then
            mask.put (j, i, 1)
          else
            mask.put (j, i, 0)
          end
          j := j + 1
        end
        i := i + 1
      end
    end
  
  mask: Int_Matrix
  matrix: Int_Matrix
end 
