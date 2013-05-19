import Prelude

class File

create open_read

  fd: Integer

  open_read(filename: String)
    do
      fd := {Prelude}.open_read(filename)
      if fd = -1 then
        {Prelude}.print("File not found")
      end
    end

  read(size: Integer): String
    local
      str: String
      read_bytes: Integer
    do
      create str.make(size)

      read_bytes := {Prelude}.fd_read(fd, str.data, size)

      if read_bytes > 0 then
        str.length := read_bytes
        Result := str
      else
        Result := Void
      end
    end

  read_all(): String
    local
      str: String
    do
      Result := ""

      from str := read(1024)
      until str = Void
      loop
        Result := Result.append(str)
        str := read(1024)
      end
    end
end
