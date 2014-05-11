class Data

create make

  value: Integer
  done: Boolean
  done_workers: Integer
  workers: Integer

  make(workers_: Integer)
    do
      value := 0
      workers := workers_
      done_workers := 0
      done := False
    end

  incr()
    do
      value := value + 1
    end

  set_done()
    do
      done_workers := done_workers + 1
      if done_workers = workers then
        done := True
      end
    end
end
