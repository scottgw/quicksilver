import Socket_Util
import String
import Prelude

class Socket

create make_server, make_fd

  fd: Integer

  make_fd(fd_in: Integer)
    do
      fd := fd_in
    end
  
  make_server(port: Integer)
    do
      fd := {Socket_Util}.new_tcp_socket()

      if fd = -1 then
        {Prelude}.print ("Could not create socket%N")
        {Prelude}.exit_with (fd)
      end
      
      if {Socket_Util}.socket_bind(fd, port) /= 0 then
        {Prelude}.print ("Could not bind socket%N")
        {Prelude}.exit_with (1)
      end
    end

  listen(backlog_size: Integer)
    do
      if {Socket_Util}.socket_listen(fd, backlog_size) /= 0 then
        {Prelude}.exit_with (1)
      end
    end

  accept(): Socket
    local
      new_socket: Socket
      new_fd: Integer
    do
      new_fd := {Socket_Util}.socket_accept(fd)
      if new_fd = -1 then
        {Prelude}.print("Error in accept%N")
        {Prelude}.exit_with(1)
      end
      create new_socket.make_fd(new_fd)
      Result := new_socket
    end
  
  recv(): String
    local
      str: String
    do
      create str.make(1024)
      {Socket_Util}.socket_recv(fd, str)

      if str.length = -1 or str.length = 0 then
        Result := Void
      else      
        Result := str
      end
    end

  close()
    do
      {Prelude}.fd_close(fd)
    end
end
