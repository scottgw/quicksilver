import String

module Prelude
  print(s: String)
    external "print"
    end

  exit_with(i: Integer)
    external "exit_with"
    end

  open_read(s: String): Integer
    external "open_read"
    end
  
  fd_close(fd: Integer)
    external "fd_close"
    end

  fd_read(fd: Integer; data: Pointer_8; size: Integer): Integer
    external "fd_read"
    end
  
  int8_to_char(i: Integer_8): Character_8
    external "int8_to_char"
    end

  int_to_str(i: Integer): String
    do
      Result := "24"
    end
end
