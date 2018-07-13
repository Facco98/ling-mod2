datatype insiemediinteri = Ins of ( int * insiemediinteri ) | Nil;

val rec unione = fn i1: insiemediinteri => fn i2: insiemediinteri =>
  let
    val rec contiene = fn Nil => (fn x: int => false)
      |Ins( a, ins) => fn x => if a = x then true else contiene ins a
  in
    fn n => ((contiene i1 n) orelse (contiene i2 n))
  end;
