import String

module Prelude
  print(s: String)
    do
      print_to_stream(1, s)
    end

  print_err(s: String)
    do
      print_to_stream(2, s)
    end

  print_int(i: Integer)
    do
      print(int_to_str(i))
    end
  
  print_to_stream(i: Integer; s:String)
    external "print_to_stream"
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

  char_to_int8(c: Character_8): Integer_8
    external "char_to_int8"
    end

  int8_to_int(i: Integer_8): Integer
    external "int8_to_int"
    end

  int_to_int8(i: Integer): Integer_8
    external "int_to_int8"
    end

  int_to_real(i: Integer): Real
    external "int_to_real"
    end

  int_to_nat32(i: Integer): Natural_32
    external "int_to_nat32"
    end

  nat32_to_int(n: Natural_32): Integer
    external "nat32_to_int"
    end


  int_to_str(i: Integer): String
    local
      str: String
      digit: Integer_8
      rest: Integer
      val_0: Integer_8
      printed_first: Boolean
    do
      str := ""
      val_0 := char_to_int8 ('0')

      from 
        rest := i
        printed_first := False
      until
        rest = 0 and printed_first
      loop
        printed_first := True
        digit := int_to_int8 (rest \\ 10)
        rest := rest / 10
        str := str.prepend_char (int8_to_char (val_0 + digit))
      end

      Result := str
    end

  real_to_str(r: Real): String
    external "real_to_str"
    end

  int_max(x: Integer; y: Integer): Integer
    do
      if x >= y then
        Result := x
      else
        Result := y
      end
    end

  
  get_int_env(s: String): Integer
    external "get_int_env"
    end


  get_time(): Real
    external "get_time"
    end
end
