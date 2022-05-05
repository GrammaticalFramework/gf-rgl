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
      insertInf {inpl = <\\_ => [], (vpi.s ! v.isAux)> ; extr = \\_ => []}  -- HL 3/22
        (predVGen v.isAux v) ;

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
      w = WLight ;
      ext, rc = []
      } ;

    DetNPFem det = {
      s = \\c => det.sp ! Fem ! c ; ---- genders
      a = agrP3 det.n ;
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
          { c1 = vp.c2 ** {c = c}} ;
    -- regulates passivised object: accusative objects -> nom; all others: same case
    -- this also gives "mit dir wird gerechnet" ;
    -- the alternative linearisation ("es wird mit dir gerechnet") is not implemented

    PassAgentVPSlash vp np = ---- "von" here, "durch" in StructuralGer
      insertObj (\\_ => (PastPartAgentAP (lin VPSlash vp) (lin NP np)).s ! APred) (predV werdenPass) ;

    Pass3V3 v = -- HL 7/19
      let bekommenPass : Verb = P.habenV (P.irregV "bekommen" "bekommt" "bekam" "bekäme" "bekommen") 
      in insertObj (\\_ => (v.s ! VPastPart APred)) (predV bekommenPass) **
           { c1 = PrepNom ; c2 = v.c2 ; objCtrl = False } ;

    PastPartAP vp =
      let a = agrP3 Sg in {
        s = \\af => (vp.nn ! a).p1 ++ (vp.nn ! a).p2 ++ (vp.nn ! a).p3 ++ vp.a2
                    ++ vp.inf.inpl.p2 ++ (vp.inf.extr ! a) ++ vp.s.s ! VPastPart af ;
        isPre = True ;
        c = <[],[]> ;
        ext = vp.ext
      } ;

    PastPartAgentAP vp np =
      let a = agrP3 Sg ;
          agent = appPrepNP P.von_Prep np
      in {
      s = \\af => (vp.nn ! a).p1 ++ (vp.nn ! a).p2 ++ (vp.nn ! a).p3
                  ++ vp.a2 ++ agent ++ vp.inf.inpl.p2
                  ++ vp.c2.s                         -- junk if not TV
                  ++ vp.ext ++ (vp.inf.extr ! a) ++ vp.s.s ! VPastPart af ;
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
          haben = verb.inf2 ;
          neg   = tm.s ++ p.s ++ vp.a1 ++ negation ! b ; -- HL 8/19 ++ vp.a1 ! b ;
          -- obj1  = (vp.nn ! agr).p1 ;
          -- obj   = (vp.nn ! agr).p2 ; 
          -- compl = obj1 ++ neg ++ obj ++ vp.a2 ; -- from EG 15/5
          obj1  = (vp.nn ! agr).p1 ++ (vp.nn ! agr).p2 ; -- refl ++ pronouns ++ light nps
          obj2  = (vp.nn ! agr).p3 ;                     -- pp-objects and heavy nps
          obj3  = (vp.nn ! agr).p4 ++ vp.adj ++ vp.a2 ;  -- pred.AP|CN|Adv, via useComp HL 6/2019
          compl = obj1 ++ neg ++ obj2 ++ obj3 ;
          infObjs = (vp.inf.inpl.p1)!agr ;               -- adapted to new VP.inf, HL 3/2022
          infPred = vp.inf.inpl.p2 ;
          infCompl : Str = case <t,a,vp.isAux> of {
              <Fut|Cond,Anter,True> => [] ;                               --# notpresent
              _ => infObjs ++ infPred } ;
          pred : {inf, infComplfin : Str} = case <t,a,vp.isAux> of {
             <Fut|Cond,Anter,True>  =>                                    --# notpresent
               {inf    = infObjs ++ haben ++ infPred ++ verb.inf ;        --# notpresent Duden 318
                infComplfin = -- es ++ wird ++ haben ++ tun ++ wollen     --# notpresent
                   infObjs ++ verb.fin ++ haben ++ infPred ++ verb.inf} ; --# notpresent
             <_,Anter,True> =>                                            --# notpresent
               {inf    = verb.inf ++ haben ;                              --# notpresent
                infComplfin = -- es ++ wird/hat/hatte ++ tun ++ wollen    --# notpresent
                   infObjs ++ verb.fin ++ infPred ++ verb.inf ++ haben} ; --# notpresent
              <Pres,_,_> =>
               {inf    = verb.inf ++ haben ;
                infComplfin = -- es zu tun ++ [] ++ [] ++ versucht
                   infCompl ++ verb.inf ++ haben ++ verb.fin}
                                                                         ; --# notpresent
              _ =>                                                         --# notpresent
               {inf    = verb.inf ++ haben ;                               --# notpresent
                infComplfin = -- es zu tun ++ versucht ++ [] ++ hat        --# notpresent
                              infCompl ++ verb.inf ++ haben ++ verb.fin}   --# notpresent
              } ;
           extra = vp.inf.extr!agr ++ vp.ext ;
        in
        case o of {
	  Main => subj ++ verb.fin ++ compl ++ infCompl ++ pred.inf ++ extra ;
	  Inv  => verb.fin ++ subj ++ compl ++ infCompl ++ pred.inf ++ extra ;
	  Subj =>             subj ++ compl ++   pred.infComplfin   ++ extra
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

    ReflPron = { s = ResGer.reflPron } ;  -- reflexively used personal pronoun, with special forms in P3 Sg

    -- In P1,P2 we might use "selbst" to define a (stronger) reflexive pronoun instead: -- HL 3/2022
    --   du kennst mich vs. ich kenne mich selbst
    --   er kennt ihn   vs. er kennt sich (selbst)
    --   sie kennen sich (selbst) =/= sie kennen einander
    -- Likewise, instead of ReflPoss we might define a reflexive possessive pronoun:
    --   du kennst meine Fehler vs. ich kenne meine eigenen Fehler
    --   er|sie|es kennt seine|ihre Fehler  vs. er|sie|es kennt seine|ihre|seine eigenen Fehler
  oper
    reflPronSelf : Agr => Case => Str = \\a => \\c => reflPron ! a ! c ++ "selbst" ;

    reflPossPron : Agr -> Number -> Gender -> Case -> Str =
      let eigen = adjForms "eigen" "eigen" in
         \a,n,g,c -> possPron a n g c ++ (eigen ! (AMod (gennum g n) c)) ;

-- implementation of some of the relevant Foc rules from Extra

  lincat 
    Foc = {s : Mood => ResGer.Tense => Anteriority => Polarity => Str} ;
	
  lin 
    FocObj np cl =
      let n = appPrepNP cl.c2 np in mkFoc n cl ;

    FocAdv adv cl = mkFoc adv.s cl ;

    FocAP ap np =
      let adj = ap.s ! APred ;
          vp = predV ResGer.sein_V ** {ext = ap.c.p1 ++ ap.c.p2 ++ ap.ext};
               -- potentially not correct analysis for all examples
               -- works for:
               -- "treu ist sie ihm"
               -- "froh ist sie dass er da ist"
               -- "stolz ist sie auf ihn"
          subj = mkSubj np vp.c1 ;
          cl = mkClause subj.p1 subj.p2 vp
      in mkFoc adj cl ;

    UseFoc t p f = {s = t.s ++ p.s ++ f.s ! t.m ! t.t ! t.a ! p.p} ;


-- extra rules to get some of the "es" alternative linearisations

  lin
    EsVV vv vp =                             -- HL 3/2022
      let inf = mkInf False Simul Pos vp ;   -- False = force extraction
          objs : Agr => Str * Str * Str * Str = \\a => <"es",[],[],[]> ;
          vps = predV vv ** { nn = objs }
      in insertExtrapos vp.ext (
           insertInf inf vps) ;

    EsV2A v2a ap s = predV v2a ** {
      nn = \\_ => <"es",[],[],[]> ;
      adj = ap.s ! APred ;
      ext = "," ++ conjThat ++ s.s ! Sub} ;

-- "es wird gelacht"; generating formal sentences

  lincat
    FClause = ResGer.VP ** {subj : ResGer.NP} ;

  lin
    VPass v =
      let vp = predV werdenPass
      in vp ** {subj = esSubj ;
		inf = vp.inf ** {s = v.s ! VPastPart APred } } ; -- construct the formal clause

    AdvFor adv fcl = fcl ** {a2 = adv.s} ;
	
	FtoCl cl = 
		let subj = mkSubj cl.subj cl.c1
		in DisToCl subj.p1 subj.p2 cl ;


  oper -- extra operations for ExtraGer

    mkFoc : Str -> Cl -> Foc = \focus, cl ->
		lin Foc {s = \\m,t,a,p => focus ++ cl.s ! m ! t ! a ! p ! Inv} ;

    esSubj : CatGer.NP = lin NP {
      s = \\_ => "es" ;
      rc, ext = [] ;
      a = Ag Neutr Sg P3 ;
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
          inf = vp.inf.inpl.p2 ++ verb.inf ;  -- not used for linearisation of Main/Inv
          infExt = vp.inf.extr ! agr ;
          extra = vp.ext ;
          inffin : Str =
            case <a,vp.isAux> of {                       
	           <Anter,True> => verb.fin ++ inf ; -- double inf   --# notpresent
                   _            => inf ++ verb.fin   --- or just auxiliary vp
            }                                            
        in
        case o of {
	    Main => subj ++ verb.fin ++ compl ++ infExt ++ verb.inf ++ extra ++ vp.inf.inpl.p2 ;
	    Inv  => verb.fin ++ compl ++ infExt ++ verb.inf ++ extra ++ vp.inf.inpl.p2 ; -- vp.inf.s ;
	    Sub  => compl ++ infExt ++ inffin ++ extra }
    		} ; 
		
		-- this function is not entirely satisfactory as largely 
		-- though not entirely duplicating mkClause in ResGer


} 
