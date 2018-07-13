val rec contaUnico = fn [] => 0
  | x::l =>
    let
      val rec contains = fn [] => (fn x => false)
        | h::l =>( fn x => if x = h then true else (contains l x) );
    in
      if (contains l x) then (contaUnico l) else (1+(contaUnico l))
    end;
