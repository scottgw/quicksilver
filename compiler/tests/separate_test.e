import Prelude
import C

module Separate_Test

  main()
    local
      s: separate C
    do
      {Prelude}.print("Pre Make%N")
      create s.make
      {Prelude}.print("Pre Separate%N")
      separate s 
        do
          {Prelude}.print("Pre foo%N")
          s.foo()

          if s.bar() = 42 then
            {Prelude}.print("found 42%N")
          else
            {Prelude}.print("no 42...%N")
          end
        end
      {Prelude}.print("Post Separate%N")
      shutdown s
    end

end
