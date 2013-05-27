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
      c_array.put(1, a_c)

      c_array.item(0).foo()
      {Prelude}.print({Prelude}.int_to_str(c_array.item(1).bar()))
    end

end
