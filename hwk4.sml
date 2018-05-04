(*found these algorithms on git hub, used designed to re implement currying*)

(*Question 1*)

fun merge_sort f nil = nil
	| merge_sort f [a] = [a]
	| merge_sort f L =
		let
			fun halve nil = ( nil , nil )
				| halve [a] = ([ a], nil )
				| halve (a :: b :: rest ) =
			let
				val (x , y) = halve rest
			in
				(a :: x , b :: y )
			end
		fun merge ( nil , x) = x
			| merge (x , nil ) = x
			| merge (a ::b , x :: y) =
				if (f(a,x)) then a :: merge (b , x :: y)
				else x :: merge (a ::b , y );
				val (x , y) = halve L
		in
			merge ( merge_sort f x , merge_sort f y)
		end;
		
		
(*Question 2*)

fun selection_sort f nil = nil
	| selection_sort f (first::rest) = 
		let
			fun search first(nil, y) = first::(selection_sort f y)
				| search first (a::b, y) =
					if (f(a, first))
					then search a (b, first::y)
						else
						search first(b, a::y)
			in
			search first(rest, nil)
			end;

		
			
(*Question 3*)

fun insertion_sort f nil = nil
	|insertion_sort f [a] = [a]
	|insertion_sort f (x::y) = 
		let
			fun insert x nil = [x]
			|insert x (a::b) = 
				if f(x, a)
				then x::a::b
				else
				a :: (insert x b)
		in
		insert x(insertion_sort f y)
		end;




































