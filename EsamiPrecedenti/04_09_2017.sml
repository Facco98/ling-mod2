datatype intonil = Nil | Int of int;
type ambiente = string -> intonil;

val ambientevuoto = fn _: string => Nil;

val lega = fn amb: ambiente => fn str: string => fn x: int => (

    fn s => if( s = str ) then Int(x) else ( amb s )

  ): ambiente;
