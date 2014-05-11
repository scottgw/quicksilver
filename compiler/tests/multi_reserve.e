import Data

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

    create setter1.make (x, y)
    create setter2.make (x, y)
    create setter3.make (x, y)

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


  end

end
