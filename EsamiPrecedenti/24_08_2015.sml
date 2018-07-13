val elementi_pari = fn list: 'a list => let
  val rec aux = fn [] => (fn index => [])
                | a::l => (fn index => if Int.mod(index, 2) = 0 then [a] @ (aux l (index+1)) else aux l (index+1));
  in
    aux list 1
  end;
