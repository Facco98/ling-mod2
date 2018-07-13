datatype currency = eur | usd | ounce_gold;
datatype money = Eur of real | Usd of real | Ounce_Gold of real;


fun convert( amount, to ) =
	let val toEur = fn
		  Eur x  => x
		| Usd x  => x/1.05
		| Ounce_Gold x => x*1113.0
	in 
	( case to of
		  eur => Eur (toEur amount)
		| usd => Usd (toEur amount * 10.5)
		| ounce_gold => Ounce_Gold (toEur amount / 1113.0)
		, to)
end;
