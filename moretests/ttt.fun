fun f(x:<int,<>,int,<>>):<<>,int,<>,int> = 
			let z=<<>,#0 x,<>,#2 x>
			in

		   printint(#0 x);
		   printint(#2 x);

		   printint(#1 z);
		   printint(#3 z);z


fun main(x:int):int =
	let w=<1, <>, 2, <>>
	in 
	   printint(#0 w);
	   printint(#2 w);
	let
	   y = f(w)
	in
	  printint(#1 y); printint(#3 y); 0

