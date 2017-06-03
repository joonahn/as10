signature TYPECHECK =
sig
  val tc : Absyn.prog -> unit
  (* if there are errors, these are reported through ErrorMsg.error *)

  val sub: Absyn.tp * Absyn.tp -> bool
  val join: (string->unit) -> Absyn.tp * Absyn.tp -> Absyn.tp
end

structure TypeCheck :> TYPECHECK =
struct

 structure A = Absyn
     
 fun list2string nil = ""
   | list2string [t] = t
   | list2string (h::t) = h ^ "," ^ list2string t

 fun tp2string A.Inttp = "int"
   | tp2string (A.Tupletp tps) = "<" ^ (list2string (map tp2string tps)) ^ ">"
   | tp2string (A.Arrowtp (tp1, tp2)) = tp2string tp1 ^ " -> " ^ tp2string tp2
   | tp2string (A.Reftp tp) = tp2string tp ^ " ref"

 type context = A.tp Symbol.table

 exception UNIMPLEMENTED

(* subtyping *)
(* sub(t1,t2) = *)
(* case (t1, t2) of
    (t1' ref, t2' ref) => 뭐시기 bottom up! 근데 TP2가 없어서 이렇게는 안될듯*)
 fun sub (t1,t2) = case t1 of
  A.Inttp => (case t2 of 
      A.Inttp => true
      | _ => false)
  | A.Reftp(r1) => (case t2 of 
      A.Reftp(r2) => (sub (r1,r2) andalso sub (r2, r1))
      | _ => false)
  | A.Arrowtp(a1,a2) => (case t2 of
      A.Arrowtp(a3,a4) => (sub (a1,a3) andalso sub(a4, a2))
      | _ => false)
  | A.Tupletp (l1) => (case t2 of
      A.Tupletp (l2) => (case l1 of 
              [] => (case l2 of
                        [] => true
                        | h2::t2 => false)
              | h1::t1 => (case l2 of 
                        [] =>  true
                        | h2::t2 => (sub(h1,h2) andalso sub(A.Tupletp(t1),A.Tupletp(t2)))))
      | _ => false)

 fun check_sub pos (tp1, tp2) = 
   if sub (tp1, tp2) then ()
   else ErrorMsg.error (pos, "TYPECHECK: Subtype error: " ^ tp2string tp1 ^ "/" ^ tp2string tp2)

(* subtype join - complain 함수의 리턴 타입 : string-> unit... *)
(*ErrorMsg.error 함수와의 타입이 다르다.(pos.string-> unit)*)
(*그러므로 join을 하려면 컴플레인 함수를 넣어야 하는데 join (++Anonymous Fun++)(t1,t2)*)
(*받아야 하는건 string이니까 unit 하게 만들어야됨. 없는건 pos가 없으니까 tc에서 가져와야됨*)
(* fn x:string=>ErrorMsg.error(???)pos는 tc expression에서 두개 들어옴 *)
 fun join complain (t1,t2) : A.tp = 
  let  fun meet complain (t1,t2) : A.tp = 
  let fun meet_decompose_type tlist1 tlist2 = (case tlist1 of
    hd1::td1 => (case tlist2 of
      hd2::td2 => meet complain (hd1,hd2)::(meet_decompose_type td1 td2)
      |[] => hd1::(meet_decompose_type td1 []))
    |[] => (case tlist2 of
      hd2::td2 => hd2::(meet_decompose_type td2 [])
      |[] => [])) 
  in
  (case t1 of
  A.Inttp => (case t2 of 
    A.Inttp => A.Inttp
    | _ => ((complain "TYPECHECK: cannot make meet");A.Tupletp([])))
  |A.Tupletp(tl1) => (case t2 of
    A.Tupletp(tl2) => A.Tupletp(meet_decompose_type tl1 tl2)
    |_ => ((complain "TYPECHECK: cannot make meet");A.Tupletp([])))
  |A.Arrowtp(ta1,ta2) => (case t2 of
    A.Arrowtp(ta3,ta4) => A.Arrowtp((join complain (ta1,ta3)),(meet complain (ta2,ta4)))
    |_ => ((complain "TYPECHECK: cannot make meet");A.Tupletp([])))
  |A.Reftp(rt1) => (case t2 of
    A.Reftp(rt2) => (if (sub(rt1, rt2) andalso sub(rt2, rt1)) then t1
    else ((complain "TYPECHECK: cannot make meet");A.Tupletp([])))
    |_ => ((complain "TYPECHECK: cannot make meet");A.Tupletp([])))
  )
  end in
  let fun decompose_type tlist1 tlist2 = (case tlist1 of
    hd1::td1 => (case tlist2 of
      hd2::td2 => join complain (hd1,hd2)::(decompose_type td1 td2)
      |[] => [])
    |[] => []) 
  in
  (case t1 of
  A.Inttp => (case t2 of 
    A.Inttp => A.Inttp
    | _ => ((complain "TYPECHECK: cannot make join");A.Tupletp([])))
  |A.Tupletp(tl1) => (case t2 of
    A.Tupletp(tl2) => A.Tupletp(decompose_type tl1 tl2)
    |_ => ((complain "TYPECHECK: cannot make join");A.Tupletp([])))
  |A.Arrowtp(ta1,ta2) => (case t2 of
    A.Arrowtp(ta3,ta4) => A.Arrowtp((meet complain (ta1,ta3)),(join complain (ta2,ta4)))
    |_ => ((complain "TYPECHECK: cannot make join");A.Tupletp([])))
  |A.Reftp(rt1) => (case t2 of
    A.Reftp(rt2) => (if (sub(rt1, rt2) andalso sub(rt2, rt1)) then t1
    else ((complain "TYPECHECK: cannot make join");A.Tupletp([])))
    |_ => ((complain "TYPECHECK: cannot make join");A.Tupletp([])))
  )
  end
  end

fun isJoinable (t1, t2) : bool = 
  if t1=A.Tupletp([]) andalso t2=A.Tupletp([]) then true
  else
  let val join1 = join (fn x => ()) (t1, t2)
      val join2 = join (fn x => ()) (t2, t1) in
    if join1=A.Tupletp([]) then (
      if join2=A.Tupletp([]) then (print ("TYPECHECK: cannot join" ^ tp2string t1 ^ "/" ^ tp2string t2 ^ "\n");false)
      else
        true)
    else true
  end


(* expression typing *)
(*context ctxt 는 처음 어사인할때 symbol table같은것. *)
(*pos 는 시작과 끝 위치.*)
 fun tc_exp ctxt pos e : A.tp =  
  let fun decompose_exp exp_list = (case exp_list of
    h::t => (tc_exp ctxt pos h)::(decompose_exp t)
    | [] => [])
  in
  let fun get_proj value tp_list = (case tp_list of
    h::t => (if value=0 then h else (get_proj (value-1) t))
    | [] => (ErrorMsg.error (pos, "TYPECHECK: projection failed: number of list is not enough"); A.Inttp))
  in
  case e of
  A.Id(id) => (case Symbol.look(ctxt, id) of
    SOME v => v
    |NONE => (ErrorMsg.error (pos, "TYPECHECK: there's no symbol in symtable"); A.Inttp))
  | A.Int(v) => A.Inttp
  | A.Op(oop, ol) => (case oop of
    A.Ref => (case ol of 
      h::t => A.Reftp(tc_exp ctxt pos h)
      |[] => (ErrorMsg.error(pos, "TYPECHECK: no type found in REF argument");A.Inttp))
    | A.Get => (case ol of
      h::t => (case tc_exp ctxt pos h of
        A.Reftp(rt) => rt
        | _ => (ErrorMsg.error(pos, "TYPECHECK: no type found in GET operator");A.Inttp))
      |[] => (ErrorMsg.error(pos, "TYPECHECK: no argument found in GET operator");A.Inttp))
    | A.Set => (case ol of
      h1::t1 => (case t1 of
        h2::t2 => (case (tc_exp ctxt pos h1) of
          A.Reftp(rt) => if rt=(tc_exp ctxt pos h2) then A.Tupletp([]) else (ErrorMsg.error(pos, "TYPECHECK: REF set type mismatch");A.Inttp)
          | _ => (ErrorMsg.error(pos, "TYPECHECK: SET operator type mismatch : not ref type");A.Inttp))
        |[] => (ErrorMsg.error(pos, "TYPECHECK: operand of SET is not enough");A.Inttp))
      |[] => (ErrorMsg.error(pos, "TYPECHECK: operand of SET is not enough");A.Inttp))
    | _ => (case ol of
      h1::t1 => (case t1 of
        h2::t2 => (if (tc_exp ctxt pos h1)=A.Inttp andalso 
          (tc_exp ctxt pos h2)=A.Inttp then A.Inttp else 
            (ErrorMsg.error(pos, "TYPECHECK: operand of BINOP type mismatch");A.Inttp))
        |[] => (ErrorMsg.error(pos, "TYPECHECK: operand of Binop is not enough");A.Inttp))
      |[] =>(ErrorMsg.error(pos, "TYPECHECK: operand of BINOP is not enough");A.Inttp)))
  | A.Tuple (tl) => A.Tupletp (decompose_exp tl)
  | A.Proj(v, pe) => (case tc_exp ctxt pos pe of
    A.Tupletp(ptl) => get_proj v ptl
    | _ => (ErrorMsg.error(pos, "TYPECHECK: operand of PROJ is not a list");A.Inttp))
  | A.If (e1,e2,e3) => (if (tc_exp ctxt pos e1)=A.Inttp andalso isJoinable((tc_exp ctxt pos e2),(tc_exp ctxt pos e3)) then
      (tc_exp ctxt pos e2) else (ErrorMsg.error(pos, "TYPECHECK: if type error");A.Inttp))
  | A.While (e1, e2) => (if (tc_exp ctxt pos e1)=A.Inttp then
      (case (tc_exp ctxt pos e2) of 
        A.Tupletp(wtl) => (case wtl of 
          [] => A.Tupletp([])
          |_ => (ErrorMsg.error(pos, "TYPECHECK: operand of WHILE is not unit");A.Inttp))
        |_ => (ErrorMsg.error(pos, "TYPECHECK: operand of WHILE type mismatch");A.Inttp)) else 
        (ErrorMsg.error(pos, "TYPECHECK: condition of WHILE is not int");A.Inttp))
  | A.Call (e1, e2) => (case (tc_exp ctxt pos e1) of
    A.Arrowtp(at1,at2) => if sub((tc_exp ctxt pos e2), at1) then at2 else (ErrorMsg.error(pos, "TYPECHECK: operand of call type mismatch");A.Inttp)
    |_ => (ErrorMsg.error(pos, "TYPECHECK: operand of CALL is not a function");A.Inttp))
  | A.Let (li, e1, e2) => 
    let val ctxt' =Symbol.enter(ctxt,li,(tc_exp ctxt pos e1))
    in tc_exp ctxt' pos e2 end
  | A.Constrain(ce,ct) => (if (tc_exp ctxt pos ce)=ct then ct else 
    (ErrorMsg.error(pos, "TYPECHECK: operand of CONSTRAIN type mismatch");A.Inttp))
  | A.Pos (ppos, pe) => tc_exp ctxt ppos pe
  end
  end

 fun tc_fundec ctxt ((pos, (f, x, tp1, tp2, exp)): A.fundec) =
 let val ctxt' = Symbol.enter(ctxt,x,tp1)
     val tp = tc_exp ctxt' pos exp
  in check_sub pos (tp, tp2)
 end 

(*프로그램을 타이핑하려고 하면 *)
(*fun f2 = ... f2를 f1에서 call 하려면 미리 펑션 type을 모아야 하는데 *)
(*이게 글로벌 컨텍스트이다.*)
 fun do_another_fun ((pos, fdec), ctxt) = 
  let val (df, dx, dtp1, dtp2, dexp) = fdec in
    Symbol.enter(ctxt,df,A.Arrowtp(dtp1,dtp2))
  end


(*main, printint 는 build_global_context 에 직접 추가해야됨!*)
 fun build_global_context (fundecs) =
  let val ctxt = (foldl do_another_fun Symbol.empty fundecs) in
    Symbol.enter(ctxt,Symbol.symbol("printint"),A.Arrowtp(A.Inttp,A.Tupletp([])))
  end

 fun tc (fundecs : A.prog)  = 
  let val ctxt = build_global_context(fundecs) 
   in app (tc_fundec ctxt) fundecs
  end 
							     
end
