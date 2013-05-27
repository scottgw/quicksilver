import C
import Array

module Array_Test

  main()
    local
      c_array: Array[C]
      a_c: C
    do
      create a_c.make
      create c_array.make(2)

      c_array.put(0, a_c)
    end

end
