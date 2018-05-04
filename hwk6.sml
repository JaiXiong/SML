datatype exp = Const of int
	| Var of string
	| Plus of exp * exp
	| Times of exp * exp
	| Pow of exp * int;
	
val e = Times (Times (Var "x", Var "y"), Plus (Var "x", Const 3));
val e1 = Pow (Var "x", 4);
val e2 = Pow(Plus(Var "x", Const 0),2);

(*Q1*)
fun print (Const x) = Int.toString x
	| print (Var (x)) = x
	| print (Plus(x,y)) = "(" ^ print(x)^ "+" ^ print(y)^ ")"
	| print (Times(x,y)) = "(" ^ print(x)^ "*" ^ print(y)^ ")"
	| print (Pow(x,y)) = "(" ^ print(x)^ "^" ^ Int.toString(y)^ ")";
(*Q2*)
fun deriv(Const x) _ = Const 0
	| deriv (Var x)str = if x = str then Const 1 else Const 0
	| deriv (Plus(x,y))str = Plus(deriv(x)str, deriv(y)str)
	| deriv (Times(x, y))str = Plus(Times(deriv(x)str,y), Times(x, deriv(y)str))
	| deriv (Pow(x, y))str = Times(Times(Const(y), Pow(x, y-1)), deriv(x)str);
(*Q3*)

fun simp (Const x) =  Const x
	| simp (Var x) = Var x
	| simp (Times(x, Const 0)) = Const 0
	| simp (Times(x, Const 1)) = x
	| simp (Times(Const 1, y)) = y
	| simp (Times(Const 0, y)) = Const 0	
	| simp (Times(x, y)) = Times(x, y)
	| simp (Plus (x, Const 0)) = x
	| simp (Plus (Const 0, y)) = y
	| simp (Plus (x, y)) = Plus(x,y)
	| simp (Pow(x, 0)) = Const 1
	| simp (Pow(x, 1)) = x
	| simp (Pow(x, y)) = Pow(x, y);
	
fun simplify (Const x) = Const x
	| simplify (Var x) = Var x
	| simplify (Plus(x, y)) = simp(Plus(simplify(x), simplify(y)))
	| simplify (Times(x, y)) = simp(Times(simplify(x), simplify(y)))
	| simplify (Pow(x, y)) = simp(Pow(simplify(x), y));


