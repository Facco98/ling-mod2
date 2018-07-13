datatype FOR = For of int * ( int -> int );

val rec eval = fn For( i, f ) => if( i <= 0 ) then ( fn x: int => x ) else ( fn x: int => f (eval(For(i-1, f)) x) );
