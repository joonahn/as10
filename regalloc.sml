signature REGALLOC = sig
(* alloc(f) returns mips code for function f that has all the temporaries
    replaced by mips regs and the load/store of spilled temps taken care of
*)
 val alloc : Mips.funcode ->  Mips.funcode 
end

structure RegAlloc :> REGALLOC =
struct
   structure M = Mips
   structure RS = Mips.RegSet
   structure IG = Liveness.IG

   fun spillCost x = 1

   fun getmove g (M.Move(r1,r2)) = IG.mk_edge g {from=r2,to=r1} 
     | getmove g _ = ()

 fun removeAlloc(map:M.allocation, reg:M.reg, itgg:IG.graph) = 
  let val allregs = RS.listItems(IG.nodes(itgg))
    fun removeAlloc_aux(omap, regs, rmreg) = 
      case regs of
        h::t => (
          case M.RegTb.look(omap, h) of
            SOME x => (
              if h=rmreg then
                removeAlloc_aux(omap, t, rmreg)
              else
                M.RegTb.enter(removeAlloc_aux(omap, t, rmreg), h, x))
            |NONE => removeAlloc_aux(omap, t, rmreg))
      |[] => M.RegTb.empty
  in
    removeAlloc_aux(map, allregs, reg)
  end

 fun removeSpill(spills:RS.set, reg:M.reg) = 
    RS.difference(spills, RS.singleton(reg))

      fun makeRegAddrMap(regs:RS.set) = 
        let val regl = RS.listItems(regs) 
            fun makeRegAddrMap_aux(regl, addr) = 
              case regl of
                h::t => (h, addr)::makeRegAddrMap_aux(t, addr + 4)
                |[] => []
        in
          makeRegAddrMap_aux(regl, 0)
        end

 fun changePrologOld (instl: M.funcode, numofspill:int) = 
    case instl of
      (h::rest) => (
        let val (funlab,block) = h in
          if (String.isSubstring "prolog" (M.lab2string funlab)) then
            ((funlab, (M.Arithi(M.Addi, M.reg("$sp"), M.reg("$sp"), M.immed(~(numofspill))))::block)::rest)
          else
            h::changePrologOld(rest, numofspill)
        end)
      |[] => []

 fun changeProlog (ilpair: M.funcode * string, numofspill:int) = 
    let val (instl, eplab) = ilpair in
      if numofspill>0 then
        case instl of
          h::rest => (
            let val (funlab,block) = h in
              if (String.isSubstring(M.lab2string funlab) eplab) then
                ((funlab, (M.Arithi(M.Addi, M.reg("$sp"), M.reg("$sp"), M.immed(~(numofspill))))::block)::rest)
              else
                (instl)
            end)
          |[] => []
        else
          instl
    end

 fun changeEpilog(instll: M.funcode, numofspilll:int) = 
    let fun changeEpilog_aux(instl: M.funcode, numofspill:int) = 
      if numofspill>0 then
        case instl of
          h::rest => (
            let val (funlab,block) = h in
              ((*print ("epilog???" ^ M.lab2string funlab ^ "\n");*)
              (if (String.isSubstring "epilog" (M.lab2string funlab)) then
                ((funlab, (M.Arithi(M.Addi, M.reg("$sp"), M.reg("$sp"), M.immed((numofspill))))::block)::rest)
              else
                h::changeEpilog_aux(rest, numofspill)))
            end)
          |[] => []
        else
          instl

    fun findEpilogLabel_aux(instl: M.funcode, numofspill:int) = 
      if numofspill>0 then
        case instl of
          h::rest => (
            let val (funlab,block) = h in
              (if (String.isSubstring "epilog" (M.lab2string funlab)) then
                (M.lab2string funlab)
              else
                findEpilogLabel_aux(rest, numofspill))
            end)
          |[] => ""
        else
          ""
      in
        (changeEpilog_aux(instll, numofspilll), findEpilogLabel_aux(instll, numofspilll))
      end


 fun instr_def_use(inst) =
    case inst of
     M.Arith2(_,rd,rs) => {def=[rd], use=[rs]}
   | M.Arith3(_,rd,rs,rt) => {def=[rd], use=[rs,rt]}
   | M.Arithi(_,rt,rs,_) => {def=[rt], use=[rs]}
   | M.Li(r,_) => {def=[r], use=[]}
   | M.La(r,_) => {def=[r], use=[]}
   | M.Lw(r,(_,ra)) => {def=[r], use=[ra]}
   | M.Sw(r,(_,ra)) => {def=[], use=[r,ra]}
   | M.Move(rd,rs) => {def=[rd], use=[rs]}
   | M.Branchz(_,r,_) => {def=[], use=[r]}
   | M.Branchu(_,r1,r2,_) => {def=[], use=[r1,r2]}
   | M.Branch(_,r1,r2,_) => {def=[], use=[r1,r2]}
   | M.Jal(_) => {def=[M.reg "$ra"], use=[]}
   | M.Jr(r,_) => {def=[], use=(r::[])}
   | M.Jalr(r1,r2,_,_) => {def=(r1 :: []), use=(r2::[])}
   | M.Syscall => {def=[], use=[]}
   | M.Nop => {def=[], use=[]}
   | M.J(_) => {def=[], use=[]}

 fun changeSpilledReg(inst:M.instruction, regaddrmap) = 
    let fun isSpilled(reg: M.reg, regaddrmapp) = 
            case regaddrmapp of
              h::t => (
                let val (regg, addr) = h in
                  if regg=reg then true
                  else isSpilled(reg, t)
                end)
              |[] => false
      in
        let fun cr (from:M.reg, to:M.reg) = 
          if isSpilled(from, regaddrmap) then
            to
          else
            from
        in
          case inst of
             M.Arith2(i,rd,rs) => M.Arith2(i, cr(rd, M.reg "$t0"), cr(rs, M.reg "$t0"))
           | M.Arith3(i,rd,rs,rt) => M.Arith3(i, cr(rd, M.reg "$t0"), cr(rs, M.reg "$t0"), cr(rt, M.reg "$t1"))
           | M.Arithi(i,rt,rs,n) => M.Arithi(i, cr(rt, M.reg "$t0"), cr(rs, M.reg "$t0"), n)
           | M.Li(r,n) => M.Li(cr(r, M.reg "$t0"), n)
           | M.La(r,lab) => M.La(cr(r, M.reg "$t0"), lab)
           | M.Lw(r,(lab,ra)) => M.Lw(cr(r, M.reg "$t0"), (lab, cr(ra, M.reg "$t0")))
           | M.Sw(r,(lab,ra)) => M.Sw(cr(r, M.reg "$t0"), (lab, cr(ra, M.reg "$t1")))
           | M.Move(rd,rs) => M.Move(cr(rd, M.reg "$t0"), cr(rs, M.reg "$t0"))
           | M.Branchz(i,r,lab) => M.Branchz(i, cr(r, M.reg "$t0"), lab)
           | M.Branchu(i,r1,r2,lab) => M.Branchu(i, cr(r1, M.reg "$t0"), cr(r2, M.reg "$t1"), lab)
           | M.Branch(i,r1,r2,lab) => M.Branch(i, cr(r1, M.reg "$t0"), cr(r2, M.reg "$t1"), lab)
           | M.J(lab) => M.J lab
           | M.Jal(lab) => M.Jal lab
           | M.Jr(r,also) => M.Jr(cr(r, M.reg "$t0"), also)
           | M.Jalr(r1,r2,use,def) => M.Jalr(cr(r1, M.reg "$t0"), cr(r2, M.reg "$t0"), use, def)
           | M.Syscall => M.Syscall
           | M.Nop => M.Nop
         end
       end

 fun changeGeneral(instl: M.funcode, regaddrmap) = 
    let fun isSpilled(reg: M.reg, regaddrmapp) = 
          case regaddrmapp of
            h::t => (
              let val (regg, addr) = h in
                if regg=reg then true
                else isSpilled(reg, t)
              end)
            |[] => false
        fun getAddr(reg: M.reg, regaddrmapp) = 
          case regaddrmapp of
            h::t => (
              let val (regg, addr) = h in
                if regg=reg then addr
                else getAddr(reg, t)
              end)
            |[] => ErrorMsg.impossible ("spilledreg " ^ M.reg2name reg ^ " don't have addr mapping!")
        fun getUses(inst:M.instruction) =
          let val {def=_, use=u} = instr_def_use(inst) in
            let val usel = (u) in
              if List.length(usel) > 2 then
                ErrorMsg.impossible ("Too much uses!")
              else (
              case usel of
                h1::t1 => (
                  case t1 of
                    h2::t2 => (SOME h1, SOME h2)
                    |[] => (SOME h1, NONE))
                |[] => (NONE, NONE))
            end
          end
        fun getDefs(inst: M.instruction) = 
          let val {def=d, use=_} = instr_def_use(inst) in
            let val defl = (d) in
              if List.length(defl) > 1 then
                ErrorMsg.impossible ("Too much defs!")
              else (
              case defl of
                h1::t1 => (SOME h1)
                |[] => (NONE))
            end
          end
    in
      let fun changeBlock(il) = 
        case il of
          inst::t => (
            let val (use1, use2) = getUses(inst) 
                val def1 = getDefs(inst) in
              let val preinst = 
                (case use1 of
                  SOME u1 => (
                    if isSpilled(u1, regaddrmap) then 
                      [M.Lw(M.reg("$t0"),(M.immed (getAddr(u1, regaddrmap)),M.reg("$sp")))]
                    else [])
                  | NONE => [])@ 
                (case use2 of
                  SOME u2 => (
                    if isSpilled(u2, regaddrmap) then
                      [M.Lw(M.reg("$t1"),(M.immed (getAddr(u2, regaddrmap)),M.reg("$sp")))]
                    else [])
                  | NONE => [])
                val postinst = 
                  (case def1 of
                    SOME d1 => (
                      if isSpilled(d1, regaddrmap) then
                        [M.Sw(M.reg("$t0"),(M.immed (getAddr(d1, regaddrmap)),M.reg("$sp")))]
                      else [])
                    | NONE => [])
              in
                preinst@[changeSpilledReg(inst, regaddrmap)]@postinst@changeBlock(t)
              end
            end)
          |[] => []
      in
        case instl of
          h::rest => (
            let val (funlab, block) = h in
              ((*(print ("changegeneral:"^M.lab2string funlab ^"\n"));*)(funlab, changeBlock(block))::changeGeneral(rest, regaddrmap))
            end)
          |[] => []
      end
    end

 fun coalescingMoveInst(instl: M.funcode, coalesced) = 
    let fun isCoalesced(r1, r2) = 
      (*val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b*)
      let fun fold_fcn (x, acc) = 
        if RS.member(x, r1) andalso RS.member(x, r2) then
          true
        else
          (acc orelse false)
      in
        List.foldl fold_fcn false coalesced
      end

    fun changeBlock(il:M.instruction list) = 
      case il of
      h::t => (
        case h of
        M.Move(r1,r2) => (
          if isCoalesced(r1, r2) then 
            changeBlock(t)
          else if r1=r2 then
            changeBlock(t)
          else
            h::changeBlock(t))
        | _ => h::changeBlock(t)
      )
      |[] => []
    in

      case instl of
        h::rest => (
          let val (funlab,block) = h in
            (funlab, changeBlock block)::coalescingMoveInst(rest, coalesced)
          end)
        |[] => []
    end




(*removeAlloc(map:M.allocation, reg:M.reg, itgg:IG.graph)*)
   fun alloc(instrL as ((funlab,block)::rest) : M.funcode) = 
   let val ig = Liveness.interference_graph instrL
       val _ = print (M.lab2string funlab ^ "\n")
       val movegraph = IG.newGraph()
       val _ = app (fn (_,l) => app (getmove movegraph) l) instrL
       val _ = print "###### Move graph\n"
       val _ = Liveness.printgraph print movegraph
       val newCallerSaved = List.map M.reg ["$a0","$a1","$a2","$a3", 
                             "$t2","$t3","$t4","$t5","$t6","$t7","$t8","$t9",
                             "$v0","$v1"]
       val palette = M.list2set (M.reg"$ra"::newCallerSaved @ M.calleeSaved)
       val coloring = Color.color {interference = ig, moves=movegraph, 
	                  spillCost = spillCost, palette=palette}
       val _ = Color.verify{complain=ErrorMsg.impossible, func=instrL, 
                            spillCost=spillCost, palette=palette, 
                            coloring=coloring}
       val _ = print "Register Allocation verified.\n"
       val {alloc,spills,coalesced} = coloring
       val nalloc = removeAlloc(alloc, M.reg "$gp", ig)
       val nspills = removeSpill(spills, M.reg "$gp")
       val numofspill = 4 * RS.numItems(nspills)
       val stackmap = makeRegAddrMap(nspills)
       val _ = (print "Spills: "; 
                RS.app (fn r => (print (M.reg2name r); print " ")) nspills;
	        print "\n")
       val coalesced_instrL = coalescingMoveInst(instrL, coalesced)
    in  
      
      changeGeneral(
        changeProlog(
          changeEpilog(
            List.map (fn (l,instrs) => (l,List.map (M.rename_regs nalloc) instrs)) coalesced_instrL, 
            numofspill), 
          numofspill), 
        stackmap)
        
  end

end