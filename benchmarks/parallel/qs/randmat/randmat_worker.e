import Int_Matrix
import Prelude

class Randmat_Worker
  
create make

  make (a_start, a_height, a_ncols: Integer; a_seed: Integer)
    do
      start := a_start
      height := a_height
      ncols := a_ncols
      seed := {Prelude}.int_to_nat32(a_seed)
      done := False
      create matrix.make (height, ncols)
    end

    
  fill_matrix()
    local
      s, lcg_a, lcg_c, rand_max: Natural_32
      i, j: Integer
    do
      time := {Prelude}.get_time()
      lcg_a := 1664525
      lcg_c := 1013904223
      rand_max := 100

      from i := start
      until i >= start + height
      loop
        s := seed + {Prelude}.int_to_nat32(i)
        from j := 0
        until j >= ncols
        loop
          s := lcg_a * s + lcg_c
          matrix.put(j, i - start, {Prelude}.nat32_to_int(s \\ rand_max))
          j := j + 1
        end
        i := i + 1
      end
      done := True
      time := {Prelude}.get_time() - time
    end

  time: Real
  done: Boolean
  matrix: Int_Matrix
  height: Integer
  start: Integer
  seed: Natural_32
  ncols: Integer
end
