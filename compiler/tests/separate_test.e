import C

module Separate_Test

  main()
    local
      s: separate C
    do
      create s.make
      separate s 
        do
          s.foo()
        end
    end

end
