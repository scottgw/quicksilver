import Socket
import String
import Prelude

module Http_Test 

  main()
    local
      s: Socket
      client: Socket
      str: String
      str2: String
    do
      create s.make_server(8888)
      s.listen(1024)
      str2 := Void

      {Prelude}.print ("Waiting for connection%N")
      client := s.accept()

      from str := client.recv()
      until str = Void or else str.starts_with("GET")
      loop
        {Prelude}.print ("Received: ");
        {Prelude}.print (str)
        str := client.recv()
      end
      
      client.close()
      s.close()
    end
end
