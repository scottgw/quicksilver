import Data

class Multi_Setter

  x: separate Data
  y: separate Data
  id: Integer
  is_done: Boolean
  
  make (a_x: separate Data; a_y: separate Data; a_id: Integer)
    do
      id := a_id
      x := a_x
      y := a_y
      is_done := False
    end

  run ()
    local
      i: Integer
    do
       from i := 1
       until i > 9999
       loop
          -- separate x do
          --   separate y do
          --      x.set_value(id * 10000 + i)
          --      y.set_value(id * 10000 + i)
          --   end
          -- end
          separate x y
            do
               x.set_value(id * 10000 + i)
               y.set_value(id * 10000 + i)
            end

          i := i + 1
       end

       is_done := True
    end
end
