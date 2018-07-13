
structure myInt = struct
  type t = int;
  val lowerThan = ( op <= );
  val equals = ( op = );
end;


signature QUEUE = sig
  type 'a queue
  exception QueueError
  val empty: 'a queue
  val isEmpty: 'a queue -> bool
  val insert: 'a * 'a queue -> 'a queue
  val pop: 'a queue -> 'a * 'a queue
end;

structure Queue =
struct
  type 'a queue = 'a list;
  exception QueueError;
  val empty = [];
  val isEmpty = fn nil => true
    | _ => false;
  val rec insert = fn (x, nil) => [x]
    | ( x, y::l ) => insert (x, l);
  val pop = fn nil => raise QueueError
    | a::l => (a, l);

end

signature SET =
sig
  type 'a set
  exception EmptySet
  val empty: ''a set
  val contains: ''a set * ''a -> bool
  val add: ''a * ''a set -> ''a set
  val remove: ''a * ''a set -> ''a set
end;

structure Set : SET =
struct

  type 'a set = 'a list;
  exception EmptySet;
  val empty = [];
  val rec contains = fn([]: ''a set, x: ''a) => false
    | (l: ''a set, x: ''a)=> if ((List.hd l) = x) then true else contains(List.tl l, x);
  val rec add  = fn( x: ''a, [] : ''a set ) => [x]
      | ( x: ''a, l: ''a set ) => if ( not (contains(l, x)) ) then (l @ [x]): ''a set else l: ''a set;
  val rec remove = fn(x: ''a, []: ''a set) => []
    |( x: ''a, (a::l): ''a set ) => if a = x then l else [a] @ (remove (x, l));

end;

fun main () = let
  val rec aux = fn (0, set) => set
    | (x, set) => aux(x-1, Set.add(x, set))
  val rec printIntSet = fn []: int Set.set => ""
    | a::l: int Set.set => (Int.toString a) ^ " " ^ (printIntSet l)
  val b = aux(19, Set.empty);
in
  print ((printIntSet b) ^ "\n")
end;
