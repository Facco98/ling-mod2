type currency = string;
type money = real * currency;


fun convert( amount, to ) =
	let val toEur = fn
		  (x, "eur") => x
		| (x, "usd") => x/1.05
		| (x, "ounce_gold") => x*1113.0
		| (_,_) => ~1.0
	in 
	( case to of
		  "eur" => toEur amount
		| "usd" => toEur amount * 10.5
		| "ounce_gold" => toEur amount / 1113.0
		| _ => ~1.0
		, to)
end;
