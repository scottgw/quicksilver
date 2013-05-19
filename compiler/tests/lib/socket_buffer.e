import String
import Socket
import Prelude

class Socket_Buffer

create make

  socket: Socket
  accum: String
  
  make(s: Socket)
    do
      socket := s
      accum := ""
    end

  read_line(): String
    local
      new_str: String
      cut_idx: Integer
    do
      cut_idx := find_cutoff (accum)

      if cut_idx = 1 then
        from
          new_str := socket.recv()
          Result := Void
        until
          Result /= Void or new_str = Void
        loop
          cut_idx := find_cutoff (new_str)

          if cut_idx = 1 then
            accum := accum.append(new_str)
            new_str := socket.recv()
          else
            Result := accum.append(new_str.substring(1, cut_idx))
            accum := new_str.substring(cut_idx, new_str.length + 1)
          end
        end

        if new_str = Void then
          -- Return what we have and set accum to Void
          -- so that the next run of read_line will return Void,
          -- and signal the caller that the client has closed the connection.
          Result := accum
          accum := Void
        end
      else
        -- The case where we already have a cutoff in the accumulated 
        -- string
        Result := accum.substring(1, cut_idx)
        accum := accum.substring(cut_idx, accum.length + 1)
      end
    end

  find_cutoff(str: String): Integer
    local
      r_idx: Integer
      n_idx: Integer
      cut_idx: Integer
    do
      if str = Void then
        Result := 1
      else
        r_idx := str.find('%R')
        n_idx := str.find('%N')

        if n_idx = r_idx + 1 then
          -- This first case works because we index starting at 1.
          cut_idx := n_idx + 1
        elseif n_idx = -1 and r_idx /= -1 then
          cut_idx := r_idx + 1
        elseif r_idx = -1 and n_idx /= -1 then
          cut_idx := n_idx + 1
        elseif r_idx < n_idx then
          cut_idx := r_idx + 1
        elseif n_idx < r_idx then
          cut_idx := n_idx + 1 
        elseif r_idx = -1 and n_idx = -1 then
          cut_idx := 1
        end

        Result := cut_idx
      end
    end
end
