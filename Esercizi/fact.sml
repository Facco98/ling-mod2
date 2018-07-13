val fact = fn x =>
	let
		fun fact_tr x res =
			if x = 0 then
				res
			else
				fact_tr (x-1) (x*res)
	in
		fact_tr x 1
end;
