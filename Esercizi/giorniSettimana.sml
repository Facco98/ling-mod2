val giorno = fn 1 => "Lunedi"
	| 2 => "Martedi"
	| 3 => "Mercoledi"
	| _ => "Giorno non valido";



fun indice i = case i of
        "Lunedi" => 1
        | "Martedi" => 2
        | "Mercoledi" => 3
        | _ => ~1;

