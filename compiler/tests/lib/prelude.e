import String

module Prelude
  print(s: String)
    external "print"
    end

  exit_with(i: Integer)
    external "exit_with"
    end

  fd_close(fd: Integer)
    external "fd_close"
    end
end
