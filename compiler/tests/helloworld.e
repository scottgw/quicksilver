import Prelude
import String

module HelloWorld

  main()
    local
      i: Integer
    do
      from i := 1
      until i > 10
      loop
        {Prelude}.print ("Hello World%N")
        i := i + 1
      end
    end

end

