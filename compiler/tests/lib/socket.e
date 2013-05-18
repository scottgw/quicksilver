import Socket_Util

class Socket

create make_server

  fd: Integer

  make_server()
    do
      fd := {Socket_Util}.new_tcp_socket()
    end

end
