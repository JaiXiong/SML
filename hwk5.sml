(*Question*)

fun zip (nil, nil) = nil
	| zip (_,nil) = nil
	| zip (nil,_) = nil
	| zip (a::b, x::y) = (a, x) :: zip(b, y);
	
fun vectorAdd (nil, nil) = nil
	| vectorAdd (_, nil) = nil
	| vectorAdd (nil, _) = nil
	| vectorAdd (a,b) = map (op +) (zip(a,b));
			(*let
				val x = zip(a, b)
			in 
			map (op +) x
			end;*)

fun svProduct (_, nil) = nil
	| svProduct (a,b) = map (fn x => x * a) b;
	
(*fun vmProduct (_, nil) = nil
	| vmProduct (nil, _) = nil
	| vmProduct (a, b) = 
		let
			fun matrix1 (nil, nil) = nil
			| matrix1 (_, nil) = nil
			| matrix1 (nil, _) = nil
			| matrix1 (a, b) = svProduct(hd a, hd b) :: matrix1(tl a, tl b)
			
			fun addthese (a, nil) = a
			| addthese (a, b) = vectorAdd(a, addthese(hd b, tl b))
			
			val result = matrix1(a,b)
		in
		(*addthese(hd matrix1 (a, b), tl matrix1 (a,b))*)
		addthese(hd result, tl result)
		end;*)

fun reduce f (a::b) = foldl f a b
		
fun vmProduct (_, nil) = nil
	| vmProduct (nil, _) = nil
	| vmProduct (a, b) = reduce (vectorAdd(a, vectorAdd(hd b, tl b) svProduct(hd a, hd b) :: vmProduct(tl a, tl b);
			

fun matrixProduct (nil, nil) = nil
	| matrixProduct (nil, _) = nil
	| matrixProduct (_, nil) = nil
	| matrixProduct (a, b) = vmProduct(hd a, b) :: matrixProduct(tl a, b);