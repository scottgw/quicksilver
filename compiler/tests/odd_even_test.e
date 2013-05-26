import Incr
import Data

module Odd_Even_Test

  main()
    local
      odd: separate Incr
      even: separate Incr
      data: separate Data
    do
      create data.make()
      create odd.make(0, data)
      create even.make(1, data)

      separate odd even
        do
          odd.run()
          even.run()
        end

      shutdown odd
      shutdown even
    end

end
