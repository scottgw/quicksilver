import Socket
import String
import Prelude

module Socket_Test

  main()
    local
      s: Socket
      client: Socket
      str: String
    do
      create s.make_server(8888)
      s.listen(1024)

      {Prelude}.print ("Waiting for connection%N")
      client := s.accept()

      from str := client.recv()
      until str = Void
      loop
        {Prelude}.print ("Received: ");
        {Prelude}.print (str)
        str := client.recv()
      end
      
      client.close()
      s.close()
    end
end
