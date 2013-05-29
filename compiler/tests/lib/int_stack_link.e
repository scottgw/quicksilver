class Int_Stack_Link

create make

  value: Integer
  next: Int_Stack_Link

  make(a_value: Integer; a_next: Int_Stack_Link)
    do
      value := a_value
      next := a_next
    end
end
