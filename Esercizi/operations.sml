datatype number = integer of int | real_number of real;

val add_numbers = fn ( integer a, integer b ) => integer ( a+b )
	| (integer a, real_number b) => real_number ( (real) a + b )
	| (real_number a, integer b) => real_number ( a + real(b) )
	| (real_number a, real_number b) => real_number ( a+b );
