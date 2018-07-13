infix :::;


val rec pow = fn (x, 0) => x
  |(x, y) => x*pow(x, y-1);

val rec radice = fn y => let
    val rec aux = fn(x, y) => if x*x > y then x-1 else aux(x+1, y)
  in
    aux(0,y)
end;


val rec copia = fn x => fn element => let
    val rec concat = fn x => fn list => fn element => if x = 0 then list else concat (x-1) (list@[element]) (element)
  in
    concat x [] element
end;


val rec simple = fn nil => []
  |(x,y)::l => [x] @ (simple l);

val rec semplifyList = fn nil => []
  | x::l => x @ (semplifyList l);

val rec eval = fn (nil, _) => 0
  | (x::l, function) => if (function x) then 1 + ( eval (l, function) ) else 0 + ( eval (l, function) );


val rec eval = fn nil => (fn function => 0)
  | x::nil => (fn function => if ( function x ) then 1 else 0)
  | x::l => fn function => if function x then 1 + (eval l function )else 0 + ( eval l function );

fun x ::: y = if x >= y then [] else [x] @ ((x+1) ::: y);
