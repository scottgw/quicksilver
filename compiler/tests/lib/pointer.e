import Pointer_8
import Pointer_64

module Pointer
  new_pointer_64 (size: Integer): Pointer_64
    external "new_pointer_64"
    end

  pointer_64_put (p: Pointer_64; i: Integer; v: Integer_64)
    external "pointer_64_put"
    end

  pointer_64_get (p: Pointer_64; i: Integer): Integer_64
    external "pointer_64_get"
  end


  pointer_real_put (p: Pointer_64; i: Integer; v: Real)
    external "pointer_real_put"
    end

  pointer_real_get (p: Pointer_64; i: Integer): Real
    external "pointer_real_get"
  end
  
  new_pointer_8 (size: Integer): Pointer_8
    external "new_pointer_8"
    end

  pointer_8_put (p: Pointer_8; i: Integer; v: Character_8)
    external "pointer_8_put"
    end

  pointer_8_get (p: Pointer_8; i: Integer): Character_8
    external "pointer_8_get"
  end
  
end
