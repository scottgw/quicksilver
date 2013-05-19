import String
import Socket

class Socket_Buffer

create make

  socket: Socket
  accum: String
  
  make(s: Socket)
    do
      socket := s
      accum := ""
    end
end
