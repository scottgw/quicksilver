import Prelude
import C

module Passive1

  main()
    local
      c: separate C
    do
      create c.make()

      separate c
        do
          {Prelude}.print_int(c.bar())
          {Prelude}.print("%N")
          passive c
            do
              c.foo()
            end
        end
      shutdown c
    end
end
