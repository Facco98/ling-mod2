datatype codice = rosso of string | giallo of string | verde of string;

val rec arriva = fn []: codice list => ( fn paziente: codice => [paziente] )
  | h::t => case h of
    rosso(_) => ( fn paziente => [h] @ (arriva t paziente) )
    | giallo(_) =>( fn rosso(s) => [rosso(s)] @ (h::t)
                      | paziente => [h]@(arriva t paziente) )
    | verde(_) => ( fn verde(s) => h::t @ [verde(s)]
                      | paziente => [paziente] @ (h::t));

val rec arriva = fn []: codice list => ( fn paziente: codice => [paziente] )
  | h::t => fn
