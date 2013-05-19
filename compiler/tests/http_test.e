import Socket
import String
import Prelude

module Http_Test 

  main()
    local
      serv: Socket
    do
      create serv.make_server(8888)
      serv.listen(1024)

      {Prelude}.print ("Waiting for connection%N")

      server_loop(serv)
      
      serv.close()
    end

  server_loop(serv: Socket)
    local
      str: String
      client: Socket
    do
      client := serv.accept()

      from str := client.recv()
      until str = Void or else str.starts_with("GET")
      loop
        {Prelude}.print ("Received: ");
        {Prelude}.print (str)
        str := client.recv()
      end

      client.close()
    end
end
