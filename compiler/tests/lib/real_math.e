module Real_Math

  sqrt (x: Real): Real
    external
      "real_sqrt"
    end

  from_int (i: Integer): Real
    external
      "real_from_int"
    end

  max (r1: Real; r2: Real): Real
    do
      if r1 >= r2 then
        Result := r1
      else
        Result := r2
      end
    end
end
