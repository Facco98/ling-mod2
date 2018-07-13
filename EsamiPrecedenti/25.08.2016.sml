type insiemediinteri = int -> bool;
val vuoto: insiemediinteri = fn x => false;

val aggiungi = fn f: insiemediinteri => fn x: int =>
  (
    fn n: int => if (n=x) then true else f n
  ): insiemediinteri;

val contiene = fn f: insiemediinteri => fn n => f n;

val rec intersezione = fn i1: insiemediinteri => fn i2: insiemediinteri =>
  (
    fn x => ((contiene i1 x) andalso (contiene i2 x))
  ): insiemediinteri;
