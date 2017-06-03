structure Color : COLOR = 
struct

 structure IG = Liveness.IG
 structure M = Mips
 structure RS = M.RegSet

 val (SOME spillCostInfinity) = Int.maxInt
 val hello = ref 0

 type coloring = {alloc: M.allocation, spills: RS.set}

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
 	let val _ = print "alloc: "
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
	     coloring={alloc: M.allocation, spills: RS.set}} : unit =
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


fun mincost (regs, costFcn, dummy) = 
	let val lregs = RS.listItems(regs)
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
 	let val allregs = RS.listItems(IG.nodes(itgg))
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
 	let val mappedreg = extractMappedReg(alloc, color, ig) 
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



 fun getfreecolor(victimm:M.reg, all:M.allocation, palettee, itgg) = 
 	let val _ = printregset palettee 
 		fun iterate_gfc(victim, remain, palette, itg:IG.graph) = 
 		case (remain) of
 			h::t => (
 				(*if RS.member((IG.adj itg victim), h) then*)
 					(*iterate_gfc(victim, t, palette, itg)*)
 				case M.RegTb.look(all, h) of
 					SOME x => (
 						if cancolor(itg, all, x, victim) then
 							x
 						else
 							iterate_gfc(victim, t, palette, itg))
 					|NONE => ErrorMsg.impossible "nonexisting reg in alloc")
 			|[] => (findvictim(palette))
 	in
 		(*(print ("getfreecolor mapToList:"));*)
 		(*printregset (RS.addList(RS.empty,(mapToList(all, itgg))));*)
 		(*print ("victim:"^M.reg2name victimm^ "\n");*)
 		(iterate_gfc(victimm, mapToList(all, itgg), palettee, itgg))
 	end

 fun colorIt(lowdeg : RS.set,
 			 highdeg: RS.set, 
 			 itg: IG.graph, 
 			 whole_palette: RS.set, 
 			 spillCost: M.reg -> int,
 			 initialalloc) = 
			  let val _ = (print "colorIt enter**************"; printregset lowdeg; printregset highdeg ) in 
 	if RS.isEmpty(lowdeg) then (
		 (* lowdeg 가 없으므로 spill 해야됨 *)
 		if RS.isEmpty(highdeg) then (
			 (* coloring finished! *)
 			({alloc=Mips.RegTb.empty, spills=RS.empty}, whole_palette))
 		else (
			 (* Actual spilling *)
 			let val _ = (print "mincost\n")
				 val victim = mincost(highdeg, spillCost, M.reg "$a0") in
 				let val newhigh = RS.delete(highdeg, victim) in
 					let val _ = (print "recalculate\n")
					 	val (newlow, newhigh2) = recalculate(newhigh, itg, RS.numItems(whole_palette)) in
 						let val ({alloc=al, spills=sp}, palette) = colorIt(newlow, newhigh2, itg, whole_palette, spillCost, initialalloc) in
 							let val newspill = RS.add(sp, victim) in
 								({alloc=al, spills=newspill}, palette)
 							end
 						end
 					end
 				end
 			end))

 	else (
 		let val victim = (findvictim(lowdeg)) in
 			let val newlow = RS.delete(lowdeg, victim) in
 				let val ({alloc=al, spills=sp}, palette) = colorIt(newlow, highdeg, itg, whole_palette, spillCost, initialalloc) in
 					if not(RS.isEmpty(palette)) then
	 					let val newcolor = getfreecolor(victim, al, palette, itg) in
	 						let val newpalette = if RS.member(palette, newcolor) then
	 							RS.delete(palette, newcolor) else palette in
	 							let val newcoloring = Mips.RegTb.enter(al, victim, newcolor) in
	 								({alloc=newcoloring, spills=sp}, newpalette)
	 							end
	 						end
	 					end
	 				else
	 					let val newspill = RS.add(sp, victim) in
	 						({alloc=al, spills=newspill}, palette)
	 					end
 				end
 			end
 		end)
		 
		 end

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
 		val prealloc = addToTable(RS.intersection(palette, IG.nodes(ig))) in
		let val (low, high) = recalculate(notprecolored, ig, RS.numItems(palette)) in
	 		let val ({alloc=al, spills=sp}, np) = colorIt(low, high, ig, prepalette, spillCost, prealloc) in
	 			printalloc(al, ig);
	 			{alloc=al, spills=sp}
	 		end
	 	end
	 end

end

