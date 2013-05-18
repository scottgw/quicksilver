import Pointer_8

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

  socket_accept(SocketFd: Integer): Integer
    external "socket_accept"
    end


  socket_recv(SocketFd: Integer; Ptr: Pointer_8; Length: Integer): Integer
    external "socket_recv"
    end

  socket_send(SocketFd: Integer; Ptr: Pointer_8; Length: Integer): Integer
    external "socket_send"
    end
end
