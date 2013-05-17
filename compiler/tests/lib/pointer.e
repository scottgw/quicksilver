import Pointer_8

module Pointer

  new_pointer_8 (size: Integer): Pointer_8
    external "new_pointer_8"
    end

  pointer_8_put (p: Pointer_8; i: Integer; v: Integer_8)
    external "pointer_8_put"
    end

end
