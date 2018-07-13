(* Dichiarazione del tipo natural *)
datatype natural = zero | successor of natural;

(* Exception *)
exception ResultIsNotNatural;
exception Undefined;
exception Impossible

(* Operatori infissi tra due natural *)
infix <;
infix <=;
infix >;
infix >=;
infix ==;

(* Somma *)
infix +;

(* Differenza *)
infix -;

(* Divisione *)
infix /;

(* Il resto della divisione *)
infix %;

(* Prodotto *)
infix *;

(* Calcola il successore di un numero naturale *)
val succ = fn zero => successor(zero)
  |x => successor(x);

(* Calcola il precedente di un numero naturale *)
val prec = fn zero => raise ResultIsNotNatural
  |(successor(x)) => x;

(* Dato un naturale lo traduce in int *)
val rec decode = fn zero => 0
  |x => 1+decode (prec x);

(* Restituisce il natural corrispondente all'intero specificato *)
val rec encode = fn 0 => zero
  |x => if x < 0 then raise ResultIsNotNatural else successor( encode (x-1) );


(* Ritorna la coppia di interi rappresentante la coppia di naturali data *)
val decodeCouple = fn (x, y) => (decode x, decode y);

(* Ritorna la coppia di naturali rappresentante la coppia di interi data *)
val encodeCouple = fn (x, y) => ( encode x, encode y );

(* Somma due numeri naturali usando la definizione di numero naturale *)
val rec sum = fn zero => (fn x  => x)
  | x => ( fn zero => x
          | y => sum (succ x) (prec y)
);

(* Calcola il prodotto di due numeri naturali utilizzandone la definizione *)
val rec product = fn zero => (fn x => zero)
  | x => ( fn zero => zero
          |y => sum x (product (x) (prec y))
);

(* Calcola la differenza tra due numeri naturali *)
val rec sub = fn zero => ( fn zero => zero
                        | x => raise ResultIsNotNatural
            )
            | x => (fn zero => x
                    | y => sub (prec x) (prec y)
            );

(* Confronta i due numeri naturali e ritorna true se il primo è strettamente minore del secondo *)
val rec isLowerThan = fn zero => (fn zero => false
                                  | x => true)
                       | x => ( fn zero => false
                                | y => isLowerThan (prec x) (prec y)
                       );

(* Confronta due numeri naturali e ritorna true se e solo se sono uguali*)
val rec equals = fn x => fn y => if isLowerThan x y then (sub y x) = zero else (sub x y) = zero;

(* Ritorna il numero naturale rappresentante il quoziente della divisione di due numeri naturali*)
val rec quotient = fn zero => (fn zero => raise Undefined
                                | x => zero)
                    | x => (fn zero => raise Impossible
                            | y => if isLowerThan x y then zero else successor(quotient (sub x y) y )
                    );

(* Ritorna il numero naturale che rappresenta il resto della divisione di due numeri naturali *)
val rec rest = fn zero => (fn zero => raise Undefined
                            |_ => zero)
              | x => (fn zero => raise Impossible
                        |y => if isLowerThan x y then x else rest (sub x y) y
              );

(* Ritorna una coppia di naturali, il primo è il quoziente mentre il secondo è il resto della divisione *)
val rec division = fn zero => (fn zero => raise Undefined
                                | x => (zero, zero) )
            | x => ( fn zero => raise Impossible
                      | y => ((quotient x y), (rest x y))
            );


(* Definizione delle funzioni degli operatori infissi *)

(* < *)
fun x < y = isLowerThan x y;

(* <= *)
fun x <= y = isLowerThan x y orelse equals x y;

(* > *)
fun x > y = (not (isLowerThan x y) andalso not (equals x y));

(* >= *)
fun x >= y = (not (isLowerThan x y));

(* == *)
fun x == y = equals x y;

(* + *)
fun x + y = sum x y;

(* - *)
fun x - y = sub x y;

(* / *)
fun x / y = division x y;

(* % *)
fun x % y = rest x y;

(* * *)
fun x * y = product x y;
