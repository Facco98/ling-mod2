datatype 'a list = empty | cons of ( 'a * 'a list );


(* Operatori infissi *)
infix contains;
infix containsList;
infix @;
infix ==;
infix ===;

(*Ritorna l'ultimo elemento della lista*)

val rec cdr = fn cons(x, empty) => x
	|cons(_, list) => cdr list;

(*Ritorna il resto della lista*)

val cdr1 = fn empty => empty
	| cons (_, list) => list;

(*Ritorna il primo elemento della lista *)

val car = fn cons (n,_) => n;

(*Ritorna true se la lista è vuota, false altrimenti*)

val  isListEmpty = fn empty => true
	| cons(x, list) => false;

(*Ritorna la lunghezza della lista*)

val rec len = fn cons(x, l) => 1 + len l
	| _  => 0;

(*Concatena due liste*)

val rec concat = fn empty => (fn l2 => l2)
	| cons(a, list) => (fn l2 => cons(a, concat list l2));

(*Mette un elemento al suo posto in una lista ordinata*)

val rec insertInOrder = fn (empty, x) => cons(x, empty)
	|(cons(a, list), x) =>
		if x < a then cons(x, cons(a,list)) else if x > a then cons(a, insertInOrder (list, x)) else cons(a, list);

(*Riordina una lista usando il metodo insertInOrder*)

val rec sort2 = fn empty => empty
	|cons(a, l) => insertInOrder ((sort2 l), a);


(* Riordina una lista senza usare il metodo insertInOrder*)
val rec sort = fn cons(x, empty) => cons(x, empty)
        |cons(x, cons(a,l)) => case sort (cons(a,l)) of
                cons(y, list) => if x < y then cons(x, cons(y, list)) else cons(y, sort(cons(x, list)))

(* Controlla se alla posizione n si trova lo stesso elemento*)

val rec checkPosition = fn (empty, empty, _) => true
	|(empty, _, _) => false
	|(_, empty, _) => false
	|(cons(a,l), cons(b,l2), n) => if n = 0 then a = b else checkPosition(l, l2, (n-1));

(* Controlla se l'elemento x è nella lista *)

val rec isIn = fn (_, empty) => false
	|(x, cons(a,l)) => if x = a then true else isIn (x, l);


(*Controlla se tutti gli elementi della lista l1 sono in l2*)
val rec allIn = fn(empty, empty) => true
	|(cons(a, l), empty) => false
	|(empty, cons(a,l)) => true
	|(cons(a, l), l2) => if isIn(a, l2) then allIn(l,l2) else false;

(* Controlla se le due liste contengono gli stessi elementi *)
val rec equals = fn(empty, empty) => true
	|(l1, l2) => allIn(l1,l2) andalso allIn(l2,l1);

(*Controlla se la lista è ordinata*)
val rec isSorted = fn cons(x, empty) => true
	|empty => true
	|cons(a, cons(b,list)) => if a <= b then isSorted (cons(b,list)) else false;

(* Controlla se tutti gli oggetti nella stessa posizione corrispondono, ovvero se le liste contengono gli stessi elementi e sono ordinate.*)
val rec samePosition = fn(empty, empty) => true
	|(cons(a, l), cons(b, l2)) => if a = b then samePosition (l, l2) else false
	|(empty, _) => false
	|(_, empty) => false;


(* Funzioni degli operatori infissi *)
fun x containsList a = allIn(a, x);
fun x contains a = isIn(a, x);
fun x @ y = concat x y;
fun x == y = equals(x, y);
fun x === y = samePosition(x, y);
