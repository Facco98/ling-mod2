val sommali = fn a: int => fn li: int list =>
  let
    val rec aux = fn nil: int list => ( fn index: int => 0 )
      |x::l => (fn index: int => if (Int.mod(index,2)) = 0 then x + (aux l (index+1)) else (aux l (index+1)));
  in
    (aux li 1) + a
  end;
