(*Question 1*)

fun mean nil = 0.0
	| mean x =
		let 
			fun sum nil = 0
				| sum x = hd x + sum(tl x)
			fun count nil = 0
				| count x = 1 + count(tl x)
		in
			real(sum x) / real(count x)
			end;
			
(*Question 2*)

fun std nil = 0.0
	| std x = 
		let
			val y = mean x
			fun helper nil = 0.0
				| helper x = ((real (hd x)) - y) * ((real (hd x)) - y) + helper(tl x)
			fun count nil = 0
				| count x = 1 + count(tl x)
		in
			Math.sqrt (helper x / real (count x))
		end;
			
			
(*Question 3*)

fun median nil = 0
	| median x = 
		let
			fun count nil = 0
				| count x = 1 + count(tl x)
			val oddeven = count x mod 2	
			fun find (nil, _) = 0
				| find (x::y, 0) = x
				| find (x::y, z) = find (y, z - 1)  
		in
			if oddeven = 0
			then (find(x, count x div 2) + find(x,(count x div 2) +1)) div 2
			else find(x, count x div 2)
		end;
		
(*Question 4*)

fun norm nil = 0.0
	| norm x = 
		let 
			val y = mean x
			val z = std x
			fun count nil = 0
				|count x = 1 + count(tl x)
			fun helper nil = 0.0
				|helper x = ((real (hd x) - y) / z)
		in 
			helper x
		end;