fun chain(x:int) : int =
	let y = 1 in
	let z = 0 in
	(y < z & y = z) & not(z = x)

fun main (argc : int) : int =

	(printint(chain(5));0)