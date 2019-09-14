concrete ExtraGer of ExtraGerAbs = CatGer ** 
  open ResGer, Coordination, Prelude, IrregGer, (P = ParadigmsGer) in {
  flags coding=utf8 ;

  lincat
    VPI   = {s : Bool => Str} ;
    [VPI] = {s1,s2 : Bool => Str} ;
  lin
    BaseVPI = twoTable Bool ;
    ConsVPI = consrTable Bool comma ;

    MkVPI vp = {s = \\b => useInfVP b vp} ;
    ConjVPI = conjunctDistrTable Bool ;

    ComplVPIVV v vpi = 
--        insertInf (vpi.s ! v.isAux) (
      insertInf {s=(vpi.s ! v.isAux);isAux=v.isAux;ctrl=SubjC} ( -- HL ??
            predVGen v.isAux v) ; ----
{-
      insertExtrapos vpi.p3 (
        insertInf vpi.p2 (
          insertObj vpi.p1 (
            predVGen v.isAux v))) ;
-}

    PPzuAdv cn = {s = case cn.g of {
      Masc | Neutr => "zum" ;
      Fem => "zur"
      } ++ cn.s ! adjfCase Weak Dat ! Sg ! Dat 
    } ;

    TImpfSubj  = {s = [] ; t = Past ; m = MConjunct} ;   --# notpresent

    moegen_VV = auxVV mögen_V ;

    ICompAP ap = {s = \\_ => "wie" ++ ap.s ! APred ; 
				  ext = ap.c.p1 ++ ap.c.p2 ++ ap.ext} ; 

    CompIQuant iq = {s = table {Ag g n p => iq.s ! n ! g ! Nom} ; ext = ""} ;

    IAdvAdv adv = {s = "wie" ++ adv.s} ;

    DetNPMasc det = {
      s = \\c => det.sp ! Masc ! c ; ---- genders
      a = agrP3 det.n ;
      -- isPron = False ;
      -- isLight = True ; 
      w = WLight ;
      ext, rc = []
      } ;

    DetNPFem det = {
      s = \\c => det.sp ! Fem ! c ; ---- genders
      a = agrP3 det.n ;
      -- isPron = False ;
      -- isLight = True ; 
      w = WLight ;
      ext, rc = []
      } ;

    EmptyRelSlash slash = {
      s = \\m,t,a,p,gn => 
          appPrep slash.c2 (\\k => usePrepC k (\c -> relPron ! gn ! c)) ++ 
          slash.s ! m ! t ! a ! p ! Sub ;
      c = (prepC slash.c2.c).c
      } ;

    PassVPSlash vp = 
		let c = case <vp.c2.c,vp.c2.isPrep> of {
               <NPC Acc,False> => NPC Nom ;
               _ => vp.c2.c}
			in insertObj (\\_ => (PastPartAP vp).s ! APred) (predV werdenPass) **
				{subjc = vp.c2 ** {c= c}} ;
		-- regulates passivised object: accusative objects -> nom; all others: same case
		-- this also gives "mit dir wird gerechnet" ;
		-- the alternative linearisation ("es wird mit dir gerechnet") is not implemented

    PassAgentVPSlash vp np = ---- "von" here, "durch" in StructuralGer
      insertObj (\\_ => (PastPartAgentAP (lin VPSlash vp) (lin NP np)).s ! APred) (predV werdenPass) ;

    Pass3V3 v = -- HL 7/19
      let bekommenPass : Verb = P.habenV (P.irregV "bekommen" "bekommt" "bekam" "bekäme" "bekommen") 
      in insertObj (\\_ => (v.s ! VPastPart APred)) (predV bekommenPass) ** { subjc = PrepNom ; c2 = v.c2 } ;      

    PastPartAP vp = {
      s = \\af => (vp.nn ! agrP3 Sg).p1 ++ (vp.nn ! agrP3 Sg).p2 ++ (vp.nn ! agrP3 Sg).p3 ++ vp.a2 ++ vp.inf.s ++ 
                  vp.ext ++ vp.infExt ++ vp.s.s ! VPastPart af ;
      isPre = True ;
      c = <[],[]> ;
      ext = [] 
      } ;

    PastPartAgentAP vp np = 
    let agent = appPrepNP P.von_Prep np
    in {
      s = \\af => (vp.nn ! agrP3 Sg).p1 ++ (vp.nn ! agrP3 Sg).p2 ++ (vp.nn ! agrP3 Sg).p3 ++ vp.a2 ++ agent ++ 
                   vp.inf.s ++ 
                   vp.c2.s ++ --- junk if not TV
                   vp.ext ++ vp.infExt ++ vp.s.s ! VPastPart af ;
      isPre = True ;
      c = <[],[]> ;
      ext = [] 
      } ;

  lincat
    VPS   = {s : Order => Agr => Str} ;
    [VPS] = {s1,s2 : Order => Agr => Str} ;

  lin
    BaseVPS = twoTable2 Order Agr ;
    ConsVPS = consrTable2 Order Agr comma ;

    PredVPS np vpi = 
      let
        subj = np.s ! NPC Nom ++ bigNP np ;
        agr  = np.a ;
      in {
        s = \\o => 
          let verb = vpi.s ! o ! agr 
          in case o of {
            Main => subj ++ verb ;
            Inv  => verb ++ subj ;   ---- älskar henne och sover jag
            Sub  => subj ++ verb 
            }
        } ;

    MkVPS tm p vp = 
      let vps = useVP vp in {
        s = \\o,agr => 
         let 
          ord   = case o of {
            Sub => True ;  -- glue prefix to verb
            _ => False
            } ;
          b = p.p ;
          a = tm.a ;
          t = tm.t ;
          m = tm.m ;
          subj  = [] ;
          verb  = vps.s  ! ord ! agr ! VPFinite m t a ;
          neg   = tm.s ++ p.s ++ vp.a1 ++ negation ! b ; -- HL 8/19 ++ vp.a1 ! b ;
          -- obj1  = (vp.nn ! agr).p1 ;
          -- obj   = (vp.nn ! agr).p2 ; 
          -- compl = obj1 ++ neg ++ obj ++ vp.a2 ; -- from EG 15/5
          obj1  = (vp.nn ! agr).p1 ++ (vp.nn ! agr).p2 ; -- refl ++ pronouns ++ nonpronouns
          obj2  = (vp.nn ! agr).p3 ;                     -- pp-objects
          obj3  = (vp.nn ! agr).p4 ++ vp.adj ++ vp.a2 ;  -- pred.AP|CN|Adv, via useComp HL 6/2019
          compl = obj1 ++ neg ++ obj2 ++ obj3 ;
          inf   = vp.inf.s ++ verb.inf ++ verb.inf2 ;
          extra = vp.ext ;
          infE : Str =                              -- HL 30/6/2019
            case <t,a,vp.isAux> of {
              <Fut|Cond,Simul,True> => inf ;                           --# notpresent
              <Fut|Cond,Anter,True> -- Duden 318: kommen wollen haben => haben kommen wollen --# notpresent
                => verb.inf2 ++ vp.inf.s ++ verb.inf ;                   --# notpresent
              <_,Anter,True> => inf ;                                  --# notpresent
              _ => verb.inf ++ verb.inf2 ++ vp.inf.s } ;
          inffin : Str =
            case <t,a,vp.isAux> of {
	           <Fut|Cond,Anter,True>  -- ... wird|würde haben kommen wollen --# notpresent
                     => verb.fin ++ verb.inf2 ++ vp.inf.s ++ verb.inf ;  --# notpresent
	           <_,Anter,True>                                      --# notpresent
                     => verb.fin ++ inf ;            -- double inf     --# notpresent
                   _ => inf ++ verb.fin              --- or just auxiliary vp
            } ;
        in
        case o of {
	    Main => subj ++ verb.fin ++ compl ++ vp.infExt ++ infE ++ extra ;
	    Inv  => verb.fin ++ subj ++ compl ++ vp.infExt ++ infE ++ extra ;
	    Sub  => subj ++ compl ++ vp.infExt ++ inffin ++ extra
          }
    } ;

    ConjVPS = conjunctDistrTable2 Order Agr ;

  lincat
    RNP = {s : Agr => Case => Str} ;
  lin
    ReflRNP vps rnp =
      insertObj (\\a => appPrep vps.c2 
        (\\k => usePrepC k (\c -> rnp.s ! a ! c))) vps ;

    ReflPoss num cn = {s = \\a,c => num.s ! cn.g ! c ++ possPron a num.n cn.g c ++ cn.s ! adjfCase Strong c ! num.n ! c} ;

-- implementation of some of the relevant Foc rules from Extra

  lincat 
	Foc = {s : Mood => ResGer.Tense => Anteriority => Polarity => Str} ;
	
  lin 
    FocObj np cl =
		let n = appPrepNP cl.c2 np
		in mkFoc n cl ; 

	FocAdv adv cl = mkFoc adv.s cl ;

	FocAP ap np =
		let adj = ap.s ! APred ;
		    vp = predV sein_V ** {ext = ap.c.p1 ++ ap.c.p2 ++ ap.ext}; 
				-- potentially not correct analysis for all examples
				-- works for:
				-- "treu ist sie ihm"
				-- "froh ist sie dass er da ist"
				-- "stolz ist sie auf ihn"
		    subj = mkSubj np vp.subjc ;
			cl = mkClause subj.p1 subj.p2 vp
		in mkFoc adj cl ;

	UseFoc t p f = {s = t.s ++ p.s ++ f.s ! t.m ! t.t ! t.a ! p.p} ;


-- extra rules to get some of the "es" alternative linearisations

  lin
	EsVV vv vp = predV vv ** {
		nn = \\a => let n = vp.nn ! a in <"es" ++ n.p1, n.p2, n.p3, n.p4, n.p5, n.p6> ;
		inf = vp.inf ** {s = vp.s.s ! (VInf True) ++ vp.inf.s} ;  -- ich genieße es zu versuchen zu gehen; alternative word order could be produced by vp.inf ++ vp.s.s... (zu gehen zu versuchen)
		a1 = vp.a1 ;
		a2 = vp.a2 ;
		ext = vp.ext ;
		infExt = vp.infExt ;
		adj = vp.adj } ;
		
	EsV2A v2a ap s = predV v2a ** {
		nn = \\_ => <"es",[],[],[],[],[]> ; 
		adj = ap.s ! APred ; 
		ext = "," ++ "dass" ++ s.s ! Sub} ;

-- "es wird gelacht"; generating formal sentences

  lincat
	FClause = ResGer.VP ** {subj : ResGer.NP} ;


  lin
	VPass v = 
  		let vp = predV werdenPass ;
		in vp ** {
			subj = esSubj ; 
			inf = vp.inf ** {s = v.s ! VPastPart APred } } ; -- construct the formal clause

	AdvFor adv fcl = fcl ** {a2 = adv.s} ;
	
	FtoCl cl = 
		let subj = mkSubj cl.subj cl.subjc 
		in DisToCl subj.p1 subj.p2 cl ;


  oper -- extra operations for ExtraGer

    mkFoc : Str -> Cl -> Foc = \focus, cl ->
		lin Foc {s = \\m,t,a,p => focus ++ cl.s ! m ! t ! a ! p ! Inv} ;

	esSubj : NP = lin NP {
		s = \\_ => "es" ; 
		rc, ext = [] ;
		a = Ag Neutr Sg P3 ;
                -- isLight = True ; 
		-- isPron = True
                w = WPron
          } ;

	DisToCl : Str -> Agr -> FClause -> Clause = \subj,agr,vp ->  
	  let vps = useVP vp in {
      s = \\m,t,a,b,o =>
        let
          ord   = case o of {
            Sub => True ;  -- glue prefix to verb
            _ => False
            } ;
          verb  = vps.s  ! ord ! agr ! VPFinite m t a ;
          neg   = vp.a1 ++ negation ! b ; -- HL 8/19 vp.a1 ! b ;
          obj1  = (vp.nn ! agr).p1 ;
          obj2  = (vp.nn ! agr).p2 ++ (vp.nn ! agr).p3 ;
          compl = obj1 ++ neg  ++ vp.adj ++ obj2 ++ vp.a2 ; -- adj added
          inf   = vp.inf.s ++ verb.inf ; -- not used for linearisation of Main/Inv
          extra = vp.ext ;
          inffin : Str = 
            case <a,vp.isAux> of {                       
	           <Anter,True> => verb.fin ++ inf ; -- double inf   --# notpresent
             _            => inf ++ verb.fin              --- or just auxiliary vp
            }                                            
        in
        case o of {
	    Main => subj ++ verb.fin ++ compl ++ vp.infExt ++ verb.inf ++ extra ++ vp.inf.s ;
	    Inv  => verb.fin ++ compl ++ vp.infExt ++ verb.inf ++ extra ++ vp.inf.s ;
	    Sub  => compl ++ vp.infExt ++ inffin ++ extra }
    		} ; 
		
		-- this function is not entirely satisfactory as largely 
		-- though not entirely duplicating mkClause in ResGer
} 
