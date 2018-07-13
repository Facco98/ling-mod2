datatype Expr = X
			         | Y
			         | Avg of Expr * Expr
			         | Mul of Expr * Expr;

val rec compute = fn X: Expr => (fn x: int => fn y: int => x)
                  | Y => ( fn x => fn y => y)
                  | Avg(e1, e2) => (fn x => fn y => Int.div(((compute e1 x y) + ( compute e2 x y )), 2))
                  | Mul(e1, e2) => (fn x => fn y => ((compute e1 x y) * ( compute e2 x y )));
