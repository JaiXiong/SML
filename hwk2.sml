(*assignment 2*)
(*people I worked with*)
(*Luka, Peter, James*)

(*problem 1*)
fun sum x = if null x then 0 else hd x + sum(tl x);

(*problem 2*)
fun count x = if null x then 0 else 1 + count(tl x);
fun mean x = if null x then 0 else sum(x) div count(x);

(*problem 3*)
fun square x = x * x;
fun help (x, y) = if null x then 0 else square(hd x - y) + help(tl x, y);
fun var x = if null x then 0 else help(x, mean(x));

(*problem 4*)
(*fun remainder x = count(x) mod 2;
fun oddeven x = if remainder(x) = 1 then (count(x) div 2)+ 1 else count(x) div 2;

fun loc (x, y) = if y = 0 then hd x else loc(tl x, y - 1);
					
fun median x = if remainder(x) = 1 then loc(x, oddeven(x)) 
				else loc(x, oddeven(x)) + loc(x, oddeven(x)) div 2; *)
				
				
(*test problem 4 without odd/even*)

fun remainder x = count(x) div 2;

fun element (x, y) = if y > remainder(x) then hd x else element(tl x, y + 1);

fun median x = if null x then 0 else element(x, 0);


