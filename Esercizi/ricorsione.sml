val xor  = fn
	(true,false) => true
	|(false,true) => true
	|(_,_) => false;


val rec fibonacci = fn (n, res2, res1) =>
	if n = 0 then res1
	else if n = 1 then res1
	else fibonacci (n-1, res1, res1+res2);
