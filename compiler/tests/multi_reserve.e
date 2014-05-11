import Data
import Multi_Setter
import Prelude

module Multi_Reserve

main()
  local
    setter1: separate Multi_Setter
    setter2: separate Multi_Setter
    setter3: separate Multi_Setter
    x: separate Data
    y: separate Data
  do
    create x.make()
    create y.make()

    create setter1.make (x, y, 1)
    create setter2.make (x, y, 2)
    create setter3.make (x, y, 3)

    separate setter1
      do
        setter1.run()
      end

    separate setter2
      do
        setter2.run()
      end

    separate setter3
      do
        setter3.run()
      end

    separate setter1
      require setter1.is_done
      do end

    separate setter2
      require setter2.is_done
      do end

    separate setter3
      require setter3.is_done
      do end

    separate x y
      do
         {Prelude}.print_int (x.value)
         {Prelude}.print (" ")
         {Prelude}.print_int (y.value)
         {Prelude}.print ("%N")
      end      

    shutdown setter3
    shutdown setter2
    shutdown setter1
    shutdown x
    shutdown y
  end

end
