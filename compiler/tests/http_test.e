import Socket
import String
import Prelude
import Socket_Buffer
import File

module Http_Test 

  main()
    local
      serv: Socket
    do
      create serv.make_server(8888)
      serv.listen(1024)

      {Prelude}.print ("http_test running%N")

      from
      until False
      loop
        serve_client(serv)
      end
      
      serv.close()
    end

  serve_client(serv: Socket)
    local
      i: Integer
      str: String
      file: String
      buffer: Socket_Buffer
      client: Socket
    do
      client := serv.accept()
      create buffer.make(client)

      str := buffer.read_line()

      if str /= Void and then str.starts_with ("GET ") then
        str := str.substring(5, str.length + 1)
        i := str.find(' ')
        if i /= -1 then
          file := str.substring(1, i)

          -- Skip past all the other stuff until we hit the end of 
          -- the request.
          from str := buffer.read_line() 
          until str = Void or str.equals("%R%N")
          loop
            str := buffer.read_line()
          end
            
          if str /= Void then
            send_file(client, file)
          end
        end
      else
        {Prelude}.print ("Unknown request: ")
        {Prelude}.print (str)
        {Prelude}.print ("%N")
      end

      client.close()
    end

  send_file(client: Socket; filename: String)
    local
      file: File
      file_contents: String
      file_length_str: String
      response: String
    do
      create file.open_read(("/home/scott/tmp").append(filename))

      file_contents := file.read_all()
      file_length_str := {Prelude}.int_to_str(file_contents.length)

      response :=
        ("HTTP/1.1 200 OK%R%N").append
        ("Content-Length: ").append
        (file_length_str).append
        ("%R%N%R%N").append
        (file_contents)

      client.send_all(response)
    end
end
