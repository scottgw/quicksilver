import Pointer_8
import String

module Socket_Util
  
  new_tcp_socket(): Integer
    external "new_tcp_socket"
    end

  socket_bind(SocketFd: Integer; Port: Integer): Integer
    external "socket_bind"
    end
  
  socket_listen(SocketFd: Integer; BacklogSize: Integer): Integer
    external "socket_listen"
    end
end
