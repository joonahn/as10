fun fibiter(n:int):int =
				     let n = ref n in let a = ref 1 in let b = ref 1 in let c = ref 0 in
							       while !n do( n := (!n) - 1; c := !a; a := !b; b := !c + !b) ; !a

fun main(x:int):int = let n = ref 1 in while (!n < 11) do (printint(fibiter(!n)) ; n := !n + 1) ; 0