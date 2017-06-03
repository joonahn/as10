functor FunLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Fun_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
structure A = Absyn
structure S = Symbol

val SymTable = S.empty

fun errorReport () = 
	let val em = ErrorMsg.error((0,0), "Type Error") in
		A.Tupletp([])
	end


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\043\000\003\000\042\000\004\000\041\000\006\000\040\000\
\\010\000\039\000\011\000\038\000\013\000\037\000\014\000\036\000\
\\018\000\035\000\021\000\034\000\024\000\033\000\029\000\032\000\000\000\
\\001\000\003\000\008\000\000\000\
\\001\000\003\000\010\000\000\000\
\\001\000\003\000\015\000\004\000\014\000\021\000\013\000\000\000\
\\001\000\003\000\065\000\000\000\
\\001\000\004\000\009\000\000\000\
\\001\000\004\000\055\000\005\000\085\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\021\000\051\000\022\000\050\000\023\000\049\000\
\\024\000\048\000\025\000\047\000\026\000\046\000\027\000\045\000\000\000\
\\001\000\004\000\055\000\005\000\086\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\021\000\051\000\022\000\050\000\023\000\049\000\
\\024\000\048\000\025\000\047\000\026\000\046\000\027\000\045\000\000\000\
\\001\000\004\000\055\000\007\000\084\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\021\000\051\000\022\000\050\000\023\000\049\000\
\\024\000\048\000\025\000\047\000\026\000\046\000\027\000\045\000\000\000\
\\001\000\004\000\055\000\009\000\091\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\021\000\051\000\022\000\050\000\023\000\049\000\
\\024\000\048\000\025\000\047\000\026\000\046\000\027\000\045\000\000\000\
\\001\000\004\000\055\000\012\000\082\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\021\000\051\000\022\000\050\000\023\000\049\000\
\\024\000\048\000\025\000\047\000\026\000\046\000\027\000\045\000\000\000\
\\001\000\005\000\018\000\011\000\017\000\019\000\016\000\000\000\
\\001\000\005\000\026\000\011\000\017\000\019\000\016\000\000\000\
\\001\000\011\000\017\000\019\000\016\000\022\000\029\000\000\000\
\\001\000\020\000\024\000\000\000\
\\001\000\020\000\080\000\000\000\
\\001\000\022\000\083\000\000\000\
\\001\000\026\000\011\000\000\000\
\\001\000\026\000\023\000\000\000\
\\001\000\030\000\006\000\000\000\
\\096\000\000\000\
\\097\000\000\000\
\\098\000\000\000\
\\099\000\004\000\055\000\015\000\054\000\016\000\053\000\017\000\052\000\
\\021\000\051\000\022\000\050\000\023\000\049\000\024\000\048\000\
\\025\000\047\000\026\000\046\000\027\000\045\000\000\000\
\\100\000\004\000\055\000\015\000\054\000\016\000\053\000\017\000\052\000\
\\021\000\051\000\022\000\050\000\023\000\049\000\024\000\048\000\
\\025\000\047\000\026\000\046\000\027\000\045\000\000\000\
\\101\000\000\000\
\\102\000\004\000\055\000\000\000\
\\103\000\004\000\055\000\021\000\051\000\022\000\050\000\023\000\049\000\
\\024\000\048\000\025\000\047\000\000\000\
\\104\000\004\000\055\000\000\000\
\\105\000\004\000\055\000\000\000\
\\106\000\004\000\055\000\023\000\049\000\000\000\
\\107\000\004\000\055\000\023\000\049\000\000\000\
\\108\000\004\000\055\000\000\000\
\\109\000\004\000\055\000\015\000\054\000\016\000\053\000\021\000\051\000\
\\022\000\050\000\023\000\049\000\024\000\048\000\025\000\047\000\000\000\
\\110\000\004\000\055\000\015\000\054\000\016\000\053\000\021\000\051\000\
\\022\000\050\000\023\000\049\000\024\000\048\000\025\000\047\000\000\000\
\\111\000\004\000\055\000\023\000\049\000\024\000\048\000\025\000\047\000\000\000\
\\112\000\004\000\055\000\023\000\049\000\024\000\048\000\025\000\047\000\000\000\
\\113\000\004\000\055\000\015\000\054\000\016\000\053\000\021\000\051\000\
\\022\000\050\000\023\000\049\000\024\000\048\000\025\000\047\000\
\\026\000\046\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\004\000\055\000\015\000\054\000\016\000\053\000\017\000\052\000\
\\021\000\051\000\022\000\050\000\023\000\049\000\024\000\048\000\
\\025\000\047\000\026\000\046\000\027\000\045\000\028\000\081\000\000\000\
\\117\000\002\000\043\000\003\000\042\000\004\000\041\000\006\000\040\000\
\\010\000\039\000\011\000\038\000\013\000\037\000\014\000\036\000\
\\018\000\035\000\021\000\034\000\024\000\033\000\029\000\032\000\000\000\
\\118\000\011\000\017\000\019\000\016\000\000\000\
\\119\000\004\000\055\000\015\000\054\000\016\000\053\000\017\000\052\000\
\\021\000\051\000\022\000\050\000\023\000\049\000\024\000\048\000\
\\025\000\047\000\026\000\046\000\027\000\045\000\000\000\
\\120\000\004\000\055\000\015\000\054\000\016\000\053\000\017\000\052\000\
\\021\000\051\000\022\000\050\000\023\000\049\000\024\000\048\000\
\\025\000\047\000\026\000\046\000\000\000\
\\121\000\004\000\055\000\015\000\054\000\016\000\053\000\017\000\052\000\
\\021\000\051\000\022\000\050\000\023\000\049\000\024\000\048\000\
\\025\000\047\000\026\000\046\000\000\000\
\\122\000\004\000\055\000\008\000\092\000\015\000\054\000\016\000\053\000\
\\017\000\052\000\021\000\051\000\022\000\050\000\023\000\049\000\
\\024\000\048\000\025\000\047\000\026\000\046\000\000\000\
\\123\000\004\000\055\000\015\000\054\000\016\000\053\000\017\000\052\000\
\\021\000\051\000\022\000\050\000\023\000\049\000\024\000\048\000\
\\025\000\047\000\026\000\046\000\000\000\
\\124\000\004\000\055\000\015\000\054\000\016\000\053\000\017\000\052\000\
\\021\000\051\000\022\000\050\000\023\000\049\000\024\000\048\000\
\\025\000\047\000\026\000\046\000\027\000\045\000\000\000\
\\125\000\000\000\
\\126\000\000\000\
\\127\000\030\000\006\000\000\000\
\\128\000\004\000\055\000\015\000\054\000\016\000\053\000\017\000\052\000\
\\021\000\051\000\022\000\050\000\023\000\049\000\024\000\048\000\
\\025\000\047\000\026\000\046\000\027\000\045\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\011\000\017\000\019\000\016\000\028\000\025\000\000\000\
\\133\000\003\000\015\000\004\000\014\000\021\000\013\000\000\000\
\\134\000\011\000\017\000\019\000\016\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\"
val actionRowNumbers =
"\020\000\050\000\052\000\054\000\
\\002\000\051\000\006\000\003\000\
\\018\000\004\000\012\000\058\000\
\\004\000\062\000\004\000\060\000\
\\019\000\015\000\057\000\013\000\
\\059\000\004\000\055\000\058\000\
\\061\000\014\000\056\000\001\000\
\\053\000\001\000\001\000\001\000\
\\042\000\001\000\001\000\001\000\
\\001\000\005\000\001\000\001\000\
\\022\000\023\000\001\000\001\000\
\\004\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\001\000\
\\001\000\001\000\025\000\030\000\
\\027\000\016\000\041\000\029\000\
\\028\000\011\000\044\000\017\000\
\\009\000\007\000\024\000\045\000\
\\043\000\031\000\032\000\033\000\
\\036\000\037\000\038\000\034\000\
\\035\000\008\000\039\000\042\000\
\\001\000\001\000\001\000\026\000\
\\021\000\040\000\048\000\010\000\
\\047\000\001\000\001\000\049\000\
\\046\000\000\000"
val gotoT =
"\
\\006\000\003\000\007\000\002\000\008\000\001\000\009\000\093\000\000\000\
\\000\000\
\\006\000\003\000\007\000\002\000\008\000\005\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\010\000\000\000\
\\000\000\
\\001\000\018\000\002\000\017\000\000\000\
\\001\000\019\000\000\000\
\\000\000\
\\001\000\020\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\025\000\000\000\
\\000\000\
\\001\000\018\000\002\000\026\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\029\000\004\000\028\000\000\000\
\\003\000\042\000\000\000\
\\003\000\029\000\004\000\054\000\000\000\
\\003\000\029\000\004\000\055\000\000\000\
\\003\000\029\000\004\000\056\000\000\000\
\\003\000\029\000\004\000\058\000\005\000\057\000\000\000\
\\003\000\029\000\004\000\059\000\000\000\
\\003\000\029\000\004\000\060\000\000\000\
\\003\000\029\000\004\000\061\000\000\000\
\\003\000\029\000\004\000\062\000\000\000\
\\000\000\
\\003\000\029\000\004\000\064\000\000\000\
\\003\000\029\000\004\000\065\000\000\000\
\\000\000\
\\000\000\
\\003\000\029\000\004\000\066\000\000\000\
\\003\000\029\000\004\000\067\000\000\000\
\\001\000\068\000\000\000\
\\003\000\029\000\004\000\069\000\000\000\
\\003\000\029\000\004\000\070\000\000\000\
\\003\000\029\000\004\000\071\000\000\000\
\\003\000\029\000\004\000\072\000\000\000\
\\003\000\029\000\004\000\073\000\000\000\
\\003\000\029\000\004\000\074\000\000\000\
\\003\000\029\000\004\000\075\000\000\000\
\\003\000\029\000\004\000\076\000\000\000\
\\003\000\029\000\004\000\077\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\000\000\
\\003\000\029\000\004\000\058\000\005\000\085\000\000\000\
\\003\000\029\000\004\000\086\000\000\000\
\\003\000\029\000\004\000\087\000\000\000\
\\003\000\029\000\004\000\088\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\003\000\029\000\004\000\091\000\000\000\
\\003\000\029\000\004\000\092\000\000\000\
\\003\000\042\000\000\000\
\\003\000\042\000\000\000\
\\000\000\
\"
val numstates = 94
val numrules = 42
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = ErrorMsg.pos
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | PROJ of unit ->  (int) | ID of unit ->  (string)
 | INT of unit ->  (int) | prog of unit ->  (A.prog)
 | fundecs of unit ->  (A.fundec list) | fundec of unit ->  (A.fundec)
 | func of unit ->  (A.func) | exps of unit ->  (A.exp list)
 | exp of unit ->  (A.exp) | oper of unit ->  (A.oper)
 | tps of unit ->  (A.tp list) | tp of unit ->  (A.tp)
end
type svalue = MlyValue.svalue
type result = A.prog
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "INT"
  | (T 2) => "ID"
  | (T 3) => "LPAREN"
  | (T 4) => "RPAREN"
  | (T 5) => "IF"
  | (T 6) => "THEN"
  | (T 7) => "ELSE"
  | (T 8) => "IN"
  | (T 9) => "LET"
  | (T 10) => "REF"
  | (T 11) => "DO"
  | (T 12) => "WHILE"
  | (T 13) => "NOT"
  | (T 14) => "OR"
  | (T 15) => "AND"
  | (T 16) => "ASSIGN"
  | (T 17) => "BANG"
  | (T 18) => "ARROW"
  | (T 19) => "GT"
  | (T 20) => "LT"
  | (T 21) => "EQ"
  | (T 22) => "TIMES"
  | (T 23) => "MINUS"
  | (T 24) => "PLUS"
  | (T 25) => "COLON"
  | (T 26) => "SEMICOLON"
  | (T 27) => "COMMA"
  | (T 28) => "PROJ"
  | (T 29) => "FUN"
  | (T 30) => "UNARY"
  | (T 31) => "IFRULE"
  | (T 32) => "FUNCCALL"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 32) $$ (T 31) $$ (T 30) $$ (T 29) $$ (T 27) $$ (T 26) $$ (T 25)
 $$ (T 24) $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18)
 $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11)
 $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ 
(T 3) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( _, _, (RPARENright as RPAREN1right))) :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, exp1left,
 _)) :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  
exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Pos((exp1left,RPARENright), A.Call (exp1,exp2)))
end)
 in ( LrTable.NT 3, ( result, exp1left, RPAREN1right), rest671)
end
|  ( 1, ( ( _, ( MlyValue.ID ID1, (IDleft as ID1left), (IDright as 
ID1right))) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (ID as ID1) = ID1 ()
 in (A.Pos((IDleft, IDright), A.Id(S.symbol(ID))))
end)
 in ( LrTable.NT 3, ( result, ID1left, ID1right), rest671)
end
|  ( 2, ( ( _, ( MlyValue.INT INT1, (INTleft as INT1left), (INTright
 as INT1right))) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (INT as INT1) = INT1 ()
 in (A.Pos((INTleft, INTright), A.Int(INT)))
end)
 in ( LrTable.NT 3, ( result, INT1left, INT1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: ( _, ( 
MlyValue.oper oper1, _, _)) :: ( _, ( MlyValue.exp exp1, exp1left, _))
 :: rest671)) => let val  result = MlyValue.exp (fn _ => let val  exp1
 = exp1 ()
 val  (oper as oper1) = oper1 ()
 val  exp2 = exp2 ()
 in (A.Pos((exp1left,exp2right), A.Op (oper,exp1::exp2::[])))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.exp exp1, _, (expright as exp1right))) :: (
 _, ( MlyValue.oper oper1, (operleft as oper1left), _)) :: rest671))
 => let val  result = MlyValue.exp (fn _ => let val  (oper as oper1) =
 oper1 ()
 val  (exp as exp1) = exp1 ()
 in (A.Pos((operleft,expright), A.Op(oper,exp::[])))
end)
 in ( LrTable.NT 3, ( result, oper1left, exp1right), rest671)
end
|  ( 5, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.exp exp1, _,
 _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 in (exp)
end)
 in ( LrTable.NT 3, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _
 => let val  (exp as exp1) = exp1 ()
 in (A.Op(A.Sub,A.Int(0)::exp::[]))
end)
 in ( LrTable.NT 3, ( result, MINUS1left, exp1right), rest671)
end
|  ( 7, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
NOT1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 in (A.If(A.Op(A.Eq, exp::A.Int(0)::[]), A.Int(1), A.Int(0)))
end)
 in ( LrTable.NT 3, ( result, NOT1left, exp1right), rest671)
end
|  ( 8, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
BANG1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 in (A.Op(A.Get,exp::[]))
end)
 in ( LrTable.NT 3, ( result, BANG1left, exp1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( 
MlyValue.PROJ PROJ1, PROJ1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (PROJ as PROJ1) = PROJ1 ()
 val  (exp as exp1) = exp1 ()
 in (A.Proj(PROJ, exp))
end)
 in ( LrTable.NT 3, ( result, PROJ1left, exp1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Op(A.Add,exp1::exp2::[]))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Op(A.Sub,exp1::exp2::[]))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Op(A.Mul,exp1::exp2::[]))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.If(exp1,A.If(exp2, A.Int(1), A.Int(0)),A.Int(0)))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.If(exp1,A.Int(1),A.If(exp2, A.Int(1), A.Int(0))))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Op(A.Eq, exp1::exp2::[]))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Op(A.LT, exp1::exp2::[]))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Op(A.Set,exp1::exp2::[]))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 18, ( ( _, ( _, _, GT1right)) :: ( _, ( MlyValue.exps exps1, _, _
)) :: ( _, ( _, LT1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exps as exps1) = exps1 ()
 in (A.Tuple(exps))
end)
 in ( LrTable.NT 3, ( result, LT1left, GT1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.exps exps1, _, exps1right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exps (fn _ => let val  (exp as exp1) = exp1 ()
 val  (exps as exps1) = exps1 ()
 in (exp::exps)
end)
 in ( LrTable.NT 4, ( result, exp1left, exps1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.exp exp1, exp1left, exp1right)) :: rest671)
) => let val  result = MlyValue.exps (fn _ => let val  (exp as exp1) =
 exp1 ()
 in (exp::[])
end)
 in ( LrTable.NT 4, ( result, exp1left, exp1right), rest671)
end
|  ( 21, ( rest671)) => let val  result = MlyValue.exps (fn _ => ([]))
 in ( LrTable.NT 4, ( result, defaultPos, defaultPos), rest671)
end
|  ( 22, ( ( _, ( MlyValue.tp tp1, _, tp1right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  (exp as exp1) = exp1 ()
 val  (tp as tp1) = tp1 ()
 in (A.Constrain(exp,tp))
end)
 in ( LrTable.NT 3, ( result, exp1left, tp1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: ( _, ( _, 
REF1left, _)) :: rest671)) => let val  result = MlyValue.exp (fn _ =>
 let val  (exp as exp1) = exp1 ()
 in (A.Op(A.Ref, exp::[]))
end)
 in ( LrTable.NT 3, ( result, REF1left, exp1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, exp1left, _)) :: rest671)) => let val  result = 
MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Let(S.symbol("JYP"),exp1,exp2))
end)
 in ( LrTable.NT 3, ( result, exp1left, exp2right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.exp exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.exp exp2, _, _)) :: _ :: ( _, ( MlyValue.exp exp1, _, _)) :: 
( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.exp
 (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in (A.If(exp1, exp2, exp3))
end)
 in ( LrTable.NT 3, ( result, IF1left, exp3right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.If(exp1, exp2, A.Tuple([])))
end)
 in ( LrTable.NT 3, ( result, IF1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671)) =>
 let val  result = MlyValue.exp (fn _ => let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.While(exp1, exp2))
end)
 in ( LrTable.NT 3, ( result, WHILE1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.exp exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.exp exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: (
 _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.exp
 (fn _ => let val  (ID as ID1) = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in (A.Let(S.symbol(ID), exp1, exp2))
end)
 in ( LrTable.NT 3, ( result, LET1left, exp2right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.fundecs fundecs1, fundecs1left, 
fundecs1right)) :: rest671)) => let val  result = MlyValue.prog (fn _
 => let val  (fundecs as fundecs1) = fundecs1 ()
 in (fundecs)
end)
 in ( LrTable.NT 8, ( result, fundecs1left, fundecs1right), rest671)

end
|  ( 30, ( ( _, ( MlyValue.fundecs fundecs1, _, fundecs1right)) :: ( _
, ( MlyValue.fundec fundec1, fundec1left, _)) :: rest671)) => let val 
 result = MlyValue.fundecs (fn _ => let val  (fundec as fundec1) = 
fundec1 ()
 val  (fundecs as fundecs1) = fundecs1 ()
 in (fundec::fundecs)
end)
 in ( LrTable.NT 7, ( result, fundec1left, fundecs1right), rest671)

end
|  ( 31, ( ( _, ( MlyValue.fundec fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.fundecs (fn _ => let val 
 (fundec as fundec1) = fundec1 ()
 in (fundec::[])
end)
 in ( LrTable.NT 7, ( result, fundec1left, fundec1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.exp exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.tp tp2, _, _)) :: _ :: _ :: ( _, ( MlyValue.tp tp1, _, _)) ::
 _ :: ( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _
, _)) :: ( _, ( _, FUN1left, _)) :: rest671)) => let val  result = 
MlyValue.func (fn _ => let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  tp1 = tp1 ()
 val  tp2 = tp2 ()
 val  (exp as exp1) = exp1 ()
 in (S.symbol(ID1), S.symbol(ID2), tp1, tp2, exp)
end)
 in ( LrTable.NT 5, ( result, FUN1left, exp1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.func func1, (funcleft as func1left), (
funcright as func1right))) :: rest671)) => let val  result = 
MlyValue.fundec (fn _ => let val  (func as func1) = func1 ()
 in (((funcleft, funcright), func))
end)
 in ( LrTable.NT 6, ( result, func1left, func1right), rest671)
end
|  ( 34, ( ( _, ( _, _, GT1right)) :: ( _, ( MlyValue.tps tps1, _, _))
 :: ( _, ( _, LT1left, _)) :: rest671)) => let val  result = 
MlyValue.tp (fn _ => let val  (tps as tps1) = tps1 ()
 in (A.Tupletp(tps))
end)
 in ( LrTable.NT 0, ( result, LT1left, GT1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.tps tps1, _, tps1right)) :: _ :: ( _, ( 
MlyValue.tp tp1, tp1left, _)) :: rest671)) => let val  result = 
MlyValue.tps (fn _ => let val  (tp as tp1) = tp1 ()
 val  (tps as tps1) = tps1 ()
 in (tp::tps)
end)
 in ( LrTable.NT 1, ( result, tp1left, tps1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.tp tp1, tp1left, tp1right)) :: rest671)) =>
 let val  result = MlyValue.tps (fn _ => let val  (tp as tp1) = tp1 ()
 in (tp::[])
end)
 in ( LrTable.NT 1, ( result, tp1left, tp1right), rest671)
end
|  ( 37, ( rest671)) => let val  result = MlyValue.tps (fn _ => ([]))
 in ( LrTable.NT 1, ( result, defaultPos, defaultPos), rest671)
end
|  ( 38, ( ( _, ( MlyValue.tp tp2, _, tp2right)) :: _ :: ( _, ( 
MlyValue.tp tp1, tp1left, _)) :: rest671)) => let val  result = 
MlyValue.tp (fn _ => let val  tp1 = tp1 ()
 val  tp2 = tp2 ()
 in (A.Arrowtp(tp1, tp2))
end)
 in ( LrTable.NT 0, ( result, tp1left, tp2right), rest671)
end
|  ( 39, ( ( _, ( _, _, REF1right)) :: ( _, ( MlyValue.tp tp1, tp1left
, _)) :: rest671)) => let val  result = MlyValue.tp (fn _ => let val 
 (tp as tp1) = tp1 ()
 in (A.Reftp(tp))
end)
 in ( LrTable.NT 0, ( result, tp1left, REF1right), rest671)
end
|  ( 40, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.tp tp1, _, _
)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = 
MlyValue.tp (fn _ => let val  (tp as tp1) = tp1 ()
 in (tp)
end)
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.tp (fn _ => let val  (ID as ID1) = ID1 ()
 in (if(ID = "int") then A.Inttp else errorReport() )
end)
 in ( LrTable.NT 0, ( result, ID1left, ID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.prog x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Fun_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun REF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun BANG (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun ARROW (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun PROJ (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.PROJ (fn () => i),p1,p2))
fun FUN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun UNARY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun IFRULE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCCALL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
end
end
