structure Color : COLOR = 
struct

 structure IG = Liveness.IG
 structure M = Mips
 structure RS = M.RegSet

 val (SOME spillCostInfinity) = Int.maxInt

 type coloring = {alloc: M.allocation, spills: RS.set, coalesced: RS.set list}

 fun printdebug str = 
 	(
 	(*print str;*)
 	())

 fun printAlias (al) = 
 	(*val newalias = (newr, [victim, x]) :: alias in*)
 	(print "##### Alias:\n";
 	case al of
 	h::t => (
 		let val (newr, rl) = h in
 			print (M.reg2name newr ^ "->");
 			List.foldl (fn (x,_) => print (M.reg2name x ^ ",")) () rl ;
 			print ("\n")
 		end
 		)
 	|[] => ())

 fun printSpill (spills) =
 	(print "#### Spills: "; 
                RS.app (fn r => (print (M.reg2name r); print " ")) spills;
	        print "\n")

 fun printregset(regs) = 
 	let val reglist = RS.listItems(regs) 
 		val _ = (print "regset : ")
 		fun mapping_fcn x = 
 			print (""^ M.reg2name x ^ " ")
	in
		(List.map mapping_fcn reglist;print "\n")
	end

 fun printreglist(regs: M.reg list) = 
 	let val reglist = regs
 		val _ = (print "reglist : ")
 		fun mapping_fcn x = 
 			print (""^ M.reg2name x ^ " ")
	in
		(List.map mapping_fcn reglist;print "\n")
	end


 fun printalloc(map:M.allocation, itgg:IG.graph) = 
 	let val _ = print "##### alloc: "
 		val allregs = RS.listItems(IG.nodes(itgg))
 		fun filter_fcn (regg: M.reg) = 
 			case M.RegTb.look(map, regg) of
 				SOME x => true
 				| NONE => false
		fun mapping_fcn (regg: M.reg) = 
			case M.RegTb.look(map, regg) of
				SOME x => print ("" ^ M.reg2name(regg) ^ "->" ^ M.reg2name(x)^" ")
				| NONE => (ErrorMsg.impossible "cannot map to list";())
	in
		let val fmap = (List.filter filter_fcn allregs) in
			(List.map mapping_fcn fmap;print "\n")
		end
 	end

 fun verify {complain: string -> unit,
             func: M.funcode, 
             spillCost: Mips.reg -> int,
             palette: RS.set,
	     coloring={alloc: M.allocation, spills: RS.set, coalesced:RS.set list}} : unit =
	let fun mention(r: M.reg) = 
		let
			val _ =	((if not (RS.member(spills, r)) then 
					case M.RegTb.look(alloc,r) of
						SOME x => (
						let
							val ee = ((if RS.member(palette, x) then ()
									else ((ErrorMsg.impossible ("invalid color used!: " ^ M.reg2name x));()));
									if RS.member(palette, r) then
										((ErrorMsg.impossible ("precolored reg is in alloc: " ^ M.reg2name r));())
									else
										()
								)
						in () end)
						| NONE => (if not(RS.member(palette,r)) then 
							((ErrorMsg.impossible ("register "^ M.reg2name r ^ " not found at both alloc/spills"));())
								else
							())
					else ());
					(if RS.member(spills, r) then
						(if RS.member(palette,r) then
							((ErrorMsg.impossible ("precolored reg is in spills: " ^ M.reg2name r));())
						else (if spillCost(r)<spillCostInfinity then () else 
							((ErrorMsg.impossible "spillCost>=Infinity!");())))
					else ())
						)
			
		in () end

		fun interfere r1 r2 = if not(r1=r2) then(
			case M.RegTb.look(alloc, r1) of
				SOME x1 => (
					case M.RegTb.look(alloc, r2) of
						SOME x2 => (
							if x1=x2 then
								((ErrorMsg.impossible ("two interfering register colored the same way!!: " ^ M.reg2name r1 ^" and "^ M.reg2name r2 ^ " => " ^ M.reg2name x1));())
							else
								()
							)
						|NONE => (
							if RS.member(palette, r2) then (
								if x1=r2 then
									((ErrorMsg.impossible ("two interfering register colored the same way!: " ^ M.reg2name x1));())
								else
									())
							else ()))
				|NONE => (
					if RS.member(palette, r1) then
						(case M.RegTb.look(alloc, r2) of
							SOME x2 => (
								if r1=x2 then
									((ErrorMsg.impossible "two register colored the same way!");())
								else
									())
							| NONE => ())
					else ())
			) else ()

	in Liveness.analyze {mention=mention,interfere=interfere} func
	end	


 fun recalculate(regss:RS.set, igss: IG.graph, num_coll:int) = 
 	let 
 		val _ = printdebug "@recalculate\n"
 		fun calculate_deg(regs:RS.set, node:M.reg, igs:IG.graph, sum:int) = 
 			case RS.listItems(regs) of
 				h::t => (
 					if RS.member((IG.adj igs node), h) then
 						calculate_deg(RS.addList(RS.empty,t), node, igs, sum + 1)
 					else
 						calculate_deg(RS.addList(RS.empty,t), node, igs, sum))
 				|[] => sum
 	in
	 	let fun iterate_it (regs, igs, remain:RS.set, low, high, num_col) = 
	 		case RS.listItems(remain) of
	 			h::t => (
	 				let val degval = calculate_deg(regs, h, igs, 0) in
	 					if degval < num_col then
	 						(iterate_it(regs, igs, RS.addList(RS.empty,t), RS.add(low, h), high, num_col))
	 					else
	 						(iterate_it(regs, igs, RS.addList(RS.empty,t), low, RS.add(high, h), num_col))
	 				end)
	 			| [] => ((*print "low:";printregset low;print " high:";printregset high;print "\n";*)(low, high))
	 	in
	 		iterate_it(regss, igss, regss, RS.empty, RS.empty, num_coll)
	 	end
 	end

 fun mapToList(map:M.allocation, itgg:IG.graph) = 
 	let val allregs = RS.listItems(IG.nodes(itgg))
 		fun filter_fcn (regg: M.reg) = 
 			case M.RegTb.look(map, regg) of
 				SOME x => true
 				| NONE => false
		fun mapping_fcn (regg: M.reg) = 
			case M.RegTb.look(map, regg) of
				SOME x => regg
				| NONE => ErrorMsg.impossible "cannot map to list"
	in
		let val fmap = (List.filter filter_fcn allregs) in
			(List.map mapping_fcn fmap)
		end
 	end


 fun findvictim (regs) = 
 	case RS.listItems(regs) of
 		h::t => h
 		|[] => ErrorMsg.impossible ("Can't find victim. no regs found")
 
 fun canFindvictim (regs) = 
 	case RS.listItems(regs) of
 		h::t => true
 		|[] => false


 fun mincost (regs, costFcn, dummy) = 
	let 
		val _ = printdebug "@mincost\n"
		val lregs = RS.listItems(regs)
		val lcost = List.map (fn x => costFcn x) lregs
		fun mincost_aux(lr, lc, acc, min) =
			case lr of 
			h::t => ( case lc of
				hc::tc => (
					if acc < hc then mincost_aux(t, tc, hc, h)
					else mincost_aux(t, tc, acc, min)
				)
				|[] => ErrorMsg.impossible ("mincost: cannot reach here\n"))
			| [] => min
		in
			mincost_aux(lregs, lcost, ~1, dummy)
		end


 fun extractMappedReg(alloc, color, itgg) = 
 	let 
 		val _ = printdebug "@extractMappedReg\n"
 		val allregs = RS.listItems(IG.nodes(itgg))
 		fun filter_fcn (regg: M.reg) = 
 			case M.RegTb.look(alloc, regg) of
 				SOME x => (
 					if x=color then true
 					else false)
 				| NONE => false
	in
		(List.filter filter_fcn allregs)
 	end

(*val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b*)
 fun cancolor(ig:IG.graph, alloc:M.allocation, color:M.reg, target:M.reg) = 
 	let 
 		val _ = printdebug "@cancolor\n"
 		val mappedreg = extractMappedReg(alloc, color, ig) 
 		fun fold_fcn (ireg:M.reg, iset:RS.set) = 
 			RS.union(IG.adj ig ireg, iset)
 	in
 		let val u = List.foldl fold_fcn RS.empty mappedreg 
 			(*val _ = printregset u *)
 				in
 			if RS.member(u, target) then false
 			else true
		end
	end


(*말은 getfreecolor이지만 사실은 이미 색칠되어있는데 쓸수 있는 색을 리턴*)
 fun getfreecolor(victimm:M.reg, all:M.allocation, palettee, itgg) = 
 	let 
 		val _ = printdebug "@getfreecolor\n"
 		(*val _ = printregset palettee *)
 		fun iterate_gfc(victim, remain, palette, itg:IG.graph) = 
 		case (remain) of
 			h::t => (
 				case M.RegTb.look(all, h) of
 					SOME x => (
						 (*이미 색칠되어 있는 색 x 를 victim에 대해서도 쓸수 있는경우 쓰겠다 *)
 						if cancolor(itg, all, x, victim) then
 							x
 						else
 							iterate_gfc(victim, t, palette, itg))
 					|NONE => ErrorMsg.impossible "nonexisting reg in alloc")
			(* 이미 색칠된 색에서 쓸 수 없는 경우 팔레트에서 새로꺼냄 *)
 			|[] => (findvictim(palette))
 	in
 		(*(print ("getfreecolor mapToList:"));*)
 		(*printregset (RS.addList(RS.empty,(mapToList(all, itgg))));*)
 		(*print ("victim:"^M.reg2name victimm^ "\n");*)
 		(iterate_gfc(victimm, mapToList(all, itgg), palettee, itgg))
 	end

(*말은 getfreecolor이지만 사실은 이미 색칠되어있는데 쓸수 있는 색을 리턴*)
 fun canPaletteHandle(victimm:M.reg, all:M.allocation, palettee, itgg) = 
 	let 
 		val _ = printdebug "@canPaletteHandle\n"
 		(*val _ = printregset palettee *)
 		fun iterate_gfc(victim, remain, palette, itg:IG.graph) = 
 		case (remain) of
 			h::t => (
 				case M.RegTb.look(all, h) of
 					SOME x => (
						 (*이미 색칠되어 있는 색 x 를 victim에 대해서도 쓸수 있는경우 쓰겠다 *)
 						if cancolor(itg, all, x, victim) then
 							true
 						else
 							iterate_gfc(victim, t, palette, itg))
 					|NONE => false)
			(* 이미 색칠된 색에서 쓸 수 없는 경우 팔레트에서 새로꺼냄 *)
 			|[] => (canFindvictim(palette))
 	in
 		(*(print ("getfreecolor mapToList:"));*)
 		(*printregset (RS.addList(RS.empty,(mapToList(all, itgg))));*)
 		(*print ("victim:"^M.reg2name victimm^ "\n");*)
 		(iterate_gfc(victimm, mapToList(all, itgg), palettee, itgg))
 	end

 fun isNonMove(r:M.reg, moveg:IG.graph) : bool = 
	 not(RS.member(IG.nodes(moveg),r))

 fun isCoalescable(itg: IG.graph, r1: M.reg, r2: M.reg, k:int) : bool =
 	let 
 		val _ = printdebug "@isCoalescable\n"
 		fun isSignificant(r: M.reg, ig: IG.graph): bool = 
	 		if RS.numItems(IG.adj ig r) >= k then true else false
		 (*val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b*)
		fun fold_fcn (x, acc) = 
			if isSignificant(x, itg) then acc + 1 else acc
		val adjNodes = (RS.listItems(RS.union(IG.adj itg r1, IG.adj itg r2)))
		val nSigNeibor = List.foldl fold_fcn 0 adjNodes 
	in
		if nSigNeibor < k then true else false
	end
 
 fun mergeTwoNodes(itg: IG.graph, r1: M.reg, r2: M.reg) : M.reg = 
 	let 
 		val _ = printdebug "@mergeTwoNodes\n"
 		val adjList = RS.listItems(RS.union(IG.adj itg r1, IG.adj itg r2))
	 	val newr = M.newReg()
		fun fold_fcn (x, acc) = 
			(IG.mk_edge acc {from=x, to=newr};acc)
		val newItg = List.foldl fold_fcn itg adjList
		val _ = printdebug "@mergeTwoNodes exit\n"
	in
		newr
	end

 fun findCoalescingTarget(itg: IG.graph, moveg: IG.graph, lowdeg:RS.set, highdeg:RS.set, r: M.reg, k:int)  =
 	let 
 		val _ = printdebug "@findCoalescingTarget\n"
 		val alldeg = RS.union(lowdeg, highdeg)
 		val candidate_tmp = (RS.difference(IG.adj moveg r, IG.adj itg r))
 		val candidate = RS.listItems(RS.intersection(candidate_tmp, alldeg))
	 	fun fold_fcn(x, acc) = 
		 	case acc of SOME(y) => SOME(y)
			 |NONE => (if isCoalescable(itg, r, x, k) then SOME(x) else NONE)
	in
		List.foldl fold_fcn NONE candidate
	end

 fun recoverAlias(alias: (M.reg * M.reg list) list, alloc: M.allocation) = 
	let 
		(*val _ = print "@recoverAlias\n"*)
		fun fold_fcn(x, acc) = 
		let val (key, rl) = x in
			case M.RegTb.look(alloc, key) of
				SOME(color) => (
					List.foldl (fn (y, accy) => Mips.RegTb.enter(accy, y, color)) 
						acc rl)
				| NONE => ErrorMsg.impossible("COLOR: origin not found!")
		end
	in
		List.foldl fold_fcn alloc alias
	end

 fun calculateCoalesced (alias: (M.reg * M.reg list) list) : RS.set list = 
 	let fun fold_fcn (x, acc) = 
 		let val (key, rl) = x 
 			val changed = ref false 
 		(*기존 list에 key 가 있는지 검사*)
 			val co = List.map (fn y => if RS.member(y, key) then 
 						(changed:=true;RS.addList(RS.delete(y, key), rl)) else y) acc in
 				if !changed then 
 					co
 				else 
 					(RS.addList(RS.empty, rl)) :: acc
 		end
 	in
 		List.foldl fold_fcn [] alias
 	end

 fun printCoalesced (co: RS.set list) = 
 	if List.length(co)=0 then
 	(print "##### No Coalesced register \n")
 	else
 	(
	 	print "##### Coalesced:\n";
	 	List.foldl (fn (x,_) => print ((List.foldl (fn (y,s) => M.reg2name y ^ s) "" (RS.listItems(x)))^"\n")) () co
 	)


 fun colorIt(lowdeg : RS.set,
 			 highdeg: RS.set, 
 			 itg: IG.graph, 
			 moveg: IG.graph,
 			 whole_palette: RS.set, 
 			 spillCost: M.reg -> int,
 			 initialalloc) = 
 	( (*debug?*)
 		(*print "lowdeg:";printregset lowdeg;print "/highdeg:";printregset highdeg;print "\n";*)
 	if RS.isEmpty(lowdeg) then (
		 (* lowdeg 가 없으므로 spill 해야됨 *)
 		if RS.isEmpty(highdeg) then (
			 (* coloring Basecase! *)
 			({alloc=Mips.RegTb.empty, spills=RS.empty}, whole_palette, [])) (**)
 		else (
			 (* Actual spilling *)
 			let val victim = mincost(highdeg, spillCost, M.reg "$a0")
 				val newhigh = RS.delete(highdeg, victim)
 				val (newlow, newhigh2) = recalculate(newhigh, itg, RS.numItems(whole_palette))
 				val ({alloc=al, spills=sp}, palette, alias) = 
				 	colorIt(newlow, newhigh2, itg, moveg, whole_palette, spillCost, initialalloc) 
 				val newspill = RS.add(sp, victim) 
			in
				({alloc=al, spills=newspill}, palette, alias) (**)
 			end))

 	else (
		 (* reduce lowdeg *)
 		let val victim = (findvictim(lowdeg)) 
		 	val k = RS.numItems(whole_palette) in
		 	(*Simplify || freeze*)
		 	if isNonMove(victim, moveg) orelse (findCoalescingTarget(itg, moveg, lowdeg, highdeg, victim, k)=NONE) then (
				let	val newlow = RS.delete(lowdeg, victim)
					val ({alloc=al, spills=sp}, palette, alias) = 
						colorIt(newlow, highdeg, itg, moveg, whole_palette, spillCost, initialalloc) in
						if canPaletteHandle(victim, al, palette, itg)  then
							let val newcolor = getfreecolor(victim, al, palette, itg) 
								val newpalette = if RS.member(palette, newcolor) then
									RS.delete(palette, newcolor) else palette 
								val newcoloring = Mips.RegTb.enter(al, victim, newcolor) in
									({alloc=newcoloring, spills=sp}, newpalette, alias) (**)
							end
						else
							let val newspill = RS.add(sp, victim) in
								({alloc=al, spills=newspill}, palette, alias) (**)
							end
				end) 
			 else (
				(*Coalesce*)
				case findCoalescingTarget(itg, moveg, lowdeg, highdeg, victim, k) of
				SOME(x) => (
					let val newr = mergeTwoNodes(itg, victim, x)
						val _ = printdebug ("@colorIt before (" ^ M.reg2name victim ^ "," ^ M.reg2name x ^ "," ^ M.reg2name newr ^ ")")
						val newlow = RS.add(RS.delete(RS.delete(lowdeg, victim), x),newr)
						val _ = printdebug "@colorIt after RS.add & RS.delete^2\n"
						val ({alloc=al, spills=sp}, palette, alias) = 
							colorIt(newlow, highdeg, itg, moveg, whole_palette, spillCost, initialalloc) in

							if canPaletteHandle(newr, al, palette, itg) then
								let 
									val _ = printdebug "@colorIt coalese target found\n"
									val newcolor = getfreecolor(newr, al, palette, itg) 
									val newpalette = if RS.member(palette, newcolor) then
										RS.delete(palette, newcolor) else palette 
									val newcoloring = Mips.RegTb.enter(al, newr, newcolor) 
									val newalias = ((newr, [victim, x]) :: alias) 
									val _ = printAlias newalias in
										({alloc=newcoloring, spills=sp}, newpalette, newalias) (**)
								end
							else
								let 
									val _ = printdebug "@colorIt coalese target found but palette is empty\n"
									val newspill = RS.add(sp, victim) in
									({alloc=al, spills=newspill}, palette, alias) (**)
								end

					end
				)
				| None => ErrorMsg.impossible("COLOR: CANNOT REACH HERE @ Colorit\n")
			 )
 		
		end))
		 

 fun addToTable(coloredreg:RS.set) = 
 	let val tbll = Mips.RegTb.empty
 		fun iterate_add(tbl, remain) = 
 			case remain of
 				h::t => iterate_add(Mips.RegTb.enter(tbl, h, h), t)
 				|[] => tbl
 	in
 		iterate_add(tbll, RS.listItems(coloredreg))
 	end

 fun color ({interference = ig: IG.graph,
             moves: IG.graph,
             spillCost: M.reg ->int,
             palette: RS.set}) : coloring =
 	let val prepalette = RS.difference(palette, IG.nodes(ig))
 		val notprecolored = RS.difference(IG.nodes(ig), palette)
 		val prealloc = addToTable(RS.intersection(palette, IG.nodes(ig)))
		val (low, high) = recalculate(notprecolored, ig, RS.numItems(palette)) 
		val ({alloc=al, spills=sp}, np, alias) = 
			 		colorIt (low, high, ig, moves, prepalette, spillCost, prealloc) 
		val newalloc = recoverAlias(alias, al)
		val co = calculateCoalesced(alias) 
		val _ = printAlias(alias)
		val _ = printCoalesced(co)
		val _ = printalloc (newalloc, ig)
		val _ = printSpill (sp) in

	 			{alloc=newalloc, spills=sp, coalesced=co}
	 	
	 end

end

