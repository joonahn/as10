fun reftest(x:int):<> =
    let y = ref x in
    printint(!y)


fun recursion (a:int) : int =
    if not (a < 0) then
       a + recursion(a-1)
    else
       0

fun add(arr:<int,int,int>) :int =
let a = #0 arr in
let b = #1 arr in
let c = #2 arr in
a+a+a+b+b+b+c+c+c+a+b+c

/* some basic test I used over the whole project */ 
fun main (argc : int) : int =
    reftest(10);
    printint(recursion(100));
    printint(add(<3,3,4>));
    1000

