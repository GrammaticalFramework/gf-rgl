concrete ExtraGer of ExtraGerAbs = CatGer ** 
  open ResGer, Coordination, Prelude, IrregGer, (P = ParadigmsGer), (N = NounGer) in {
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

    CompIQuant iq = {s = table {a => iq.s ! (gennum (genderAgr a) (numberAgr a))! Nom} ; ext = ""} ;

    IAdvAdv adv = {s = "wie" ++ adv.s} ;

    DetNPMasc det = {
      s = \\b,c => det.sp ! b ! Masc ! c ;
      a = agrgP3 Masc det.n ;
      w = case det.isDef of {True => WLight ; _ => WHeavy} ;
      ext, rc = []
      } ;

    DetNPFem det = {
      s = \\b,c => det.sp ! b ! Fem ! c ;
      a = agrgP3 Fem det.n ;
      w = case det.isDef of {True => WLight ; _ => WHeavy} ;--WLight ;
      ext, rc = []
      } ;

    EmptyRelSlash slash = {
      s = \\m,t,a,p,gn => 
          appPrep slash.c2 (relPron ! gn) ++ slash.s ! m ! t ! a ! p ! Sub ;
      c = slash.c2.c
      } ;

    PassVPSlash vp = 
      insertObj (\\_ => (PastPartAP vp).s ! APred) (predV werdenPass) **
          { c1 = subjPrep vp.c2 } ;
    -- this also gives "mit dir wird gerechnet" ;
    -- the alternative linearisation ("es wird mit dir gerechnet") is not implemented

    PassAgentVPSlash vp np = ---- "von" here, "durch" in StructuralGer
      insertObj (\\_ => (PastPartAgentAP (lin VPSlash vp) (lin NP np)).s ! APred) (predV werdenPass) ;

    Pass3V3 v = -- HL 7/19
      let bekommen : Verb = P.habenV (P.irregV "bekommen" "bekommt" "bekam" "bekäme" "bekommen")
      in insertObj (\\_ => (v.s ! VPastPart APred)) (predV bekommen) **
           { c1 = PrepNom ; c2 = v.c2 ; objCtrl = False } ;

    PastPartAP vp =
      let a = agrP3 Sg in {
        s = \\af => (vp.nn ! a).p1 ++ (vp.nn ! a).p2 ++ (vp.nn ! a).p3 ++ vp.a2 ++ vp.adj
                    ++ vp.inf.inpl.p2 ++ (vp.inf.extr ! a) ++ vp.s.s ! VPastPart af ;
        s2 = \\_ => [] ;
        isPre = True ;
        c = <[],[]> ;
        ext = vp.ext
      } ;

    PastPartAgentAP vp np =
      let a = agrP3 Sg ;
          agent = appPrepNP P.von_Prep np
      in {
      s = \\af => (vp.nn ! a).p1 ++ (vp.nn ! a).p2 ++ (vp.nn ! a).p3
                  ++ vp.a2 ++ agent ++ vp.adj ++ vp.inf.inpl.p2
                  ++ vp.c2.s ! GPl                      -- junk if not TV
                  ++ vp.ext ++ (vp.inf.extr ! a) ++ vp.s.s ! VPastPart af ;
      s2 = \\_ => [] ;
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
        subj = np.s ! False ! Nom ++ bigNP np ;
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
          vagr = agr2vagr agr ;
          b = p.p ;
          a = tm.a ;
          t = tm.t ;
          m = tm.m ;
          subj  = [] ++ tm.s ++ p.s ;
          verb  = vps.s  ! ord ! vagr ! VPFinite m t a ;
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

-- Reflexive noun phrases -- (HL 5/2022: improved and completed, RNPList added)

  lincat
    RNP = {s : Agr => Case => Str ; rc,ext : Str ; isPron : Bool} ;
    RNPList = {s1,s2 : Agr => Case => Str} ;

  linref
    RNP = \rnp -> rnp.s ! AgSgP3 Masc ! Acc ++ rnp.ext ++ rnp.rc ;

  lin
    ReflRNP vps rnp =
      insertObjReflNP rnp vps ;

    ReflPron = { -- with personal pronoun nominative
      s = ResGer.reflPron ; rc,ext = [] ; isPron = True } ;

    -- We might define ReflPron by the stronger reflPronSelf below, using "selbst"
    -- to distinguish personal pronoun from reflexive pronoun:
    --   du kennst mich vs. ich kenne mich selbst
    --   er kennt ihn   vs. er kennt sich (selbst)
    --   sie kennen sich (selbst) =/= sie kennen einander

    ReflPoss num cn =
      {s = \\a,c => let adjf = case num.n of {Sg => Strong ; Pl => Weak} -- Duden 477, HL 5/2022
         in possPron a num.n cn.g c ++ num.s ! AMod (gennum cn.g num.n) c -- HL 5/2022: meine wenigstens 3 cn,
            ++ cn.s ! adjfCase adjf c ! num.n ! c       --       not: wenigstens 3 meine cn
            ++ cn.adv ;
       ext = cn.ext ; rc = cn.rc ! num.n ;
       isPron = False} ;

    -- We might define ReflPoss by the stronger reflPossPron below, using "eigen(er)"
    -- to distinguish possessive pronoun from reflexive possessive pronoun:
    --   du kennst meine Fehler vs. ich kenne meine eigenen Fehler
    --   er|sie|es kennt seine|ihre Fehler  vs. er|sie|es kennt seine|ihre|seine eigenen Fehler

    PredetRNP pred rnp = rnp ** {                        -- HL 5/2022
      s = \\a,c => let n : Number = case pred.a of {PAg n => n ; _ => numberAgr a} ;
                       g = genderAgr a ;
                       d = case pred.c.k of {NoCase => c ; PredCase k => k} ;
        in case rnp.isPron of {
          True => pred.s ! Pl ! Masc ! c ++ "von" ++ rnp.s ! a ! Dat ;
          _ => pred.s ! n ! genderAgr a ! c ++ pred.c.p ++ rnp.s ! a ! d} ;
      ext = rnp.ext ; rc = rnp.rc ;
      isPron = False} ;
      -- ok: alle von uns; die meisten von uns ; wrong: *nur von uns =/= nur wir

    AdvRNP np prep rnp = {s = \\a,c => np.s ! False ! c
                            ++ appPrep prep (rnp.s ! a) ++ rnp.ext ++ rnp.rc ;
                          ext = np.ext ; rc = np.rc ; isPron = False} ;

    AdvRAP ap prep rnp =
      let                                         -- ? adv ++ ap.s ! af
        adv = appPrep prep (rnp.s ! agrP3 Sg) ;   -- bug: fixed agreement
      in ap ** { s = \\af => ap.s ! af ++ adv } ; -- e.g. unknown in one's youth

    ReflA2RNP adj rnp = -- would need AP.c : Agr => Str*Str, not AP.c : Str*Str
      let                                            -- as we have no reflexive AP,
        compl = appPrep adj.c2 (rnp.s ! agrP3 Sg) ;  -- we use a fixed agreement
      in {
        s = adj.s ! Posit ;
        s2 = \\_ => [] ;
        isPre = True ;
        c = case adj.c2.t of {isCase => <compl, []> ; _ => <[], compl>} ;
        ext = rnp.ext ++ rnp.rc
      } ;

    PossPronRNP pron num cn rnp =
      N.DetCN (N.DetQuant (N.PossPron pron) num)
      (N.PossNP cn (lin NP {s = \\_,c => rnp.s ! pron.a ! c ;
                            a = pron.a ;
                            w = WLight ;
                            ext = rnp.ext ;
                            rc = rnp.rc})) ;

    -- AdvRVP : VP -> Prep -> RNP -> VP not implemented, as the reflexive adverb (Prep + RNP): Agr => Str
    -- could only be added to vp.a2:Str with fixed agreement, but can depend on nominal subject or object,
    -- e.g. "er spricht mit ihr über sein Kind" vs. "er spricht mit ihr über ihr Kind".

    ConjRNP conj rnps = conjunctDistrTable2 Agr Case conj rnps
      ** {isPron = False ; ext,rc = []} ;

    Base_rr_RNP x y = twoTable2 Agr Case x y ;
    Base_nr_RNP x y = twoTable2 Agr Case {s = \\_,c => x.s ! False ! c ++ x.ext ++ x.rc} y ;
    Base_rn_RNP x y = twoTable2 Agr Case x {s = \\_,c => y.s ! False ! c ++ y.ext ++ y.rc} ;

    Cons_rr_RNP x xs = consrTable2 Agr Case comma x xs ;
    Cons_nr_RNP x xs = consrTable2 Agr Case comma {s = \\_,c => x.s ! False ! c ++ x.ext ++ x.rc} xs ;

  oper
    reflPronSelf : Agr => Case => Str = \\a => \\c => reflPron ! a ! c ++ "selbst" ;

    reflPossPron : Agr -> Number -> Gender -> Case -> Str =
      let eigen = adjForms "eigen" "eigen" in
         \a,n,g,c -> possPron a n g c ++ (eigen ! (AMod (gennum g n) c)) ;

    insertObjReflNP : RNP -> ResGer.VPSlash -> ResGer.VP = -- HL 5/2022
      \rnp,vp -> insertObjRNP rnp vp.c2 vp ;

    insertObjRNP : RNP -> Preposition -> ResGer.VPSlash -> ResGer.VP = -- HL 5/2022
      \rnp,prep,vp ->                                           -- generalize ResGer.insertObjRefl
      let
        obj : Agr => Str = \\a => prep.s ! GPl ++ rnp.s ! a ! prep.c ++ rnp.ext ++ rnp.rc
      in vp ** {
        nn = \\a =>
          let vpnn = vp.nn ! a in
          case <prep.t, rnp.isPron, prep.c> of {      -- consider non-pron rnp as light, add to vpnn.p2
            <isCase,True,Acc> => <obj ! a ++ vpnn.p1, vpnn.p2, vpnn.p3, vpnn.p4> ; -- pronoun switch:
            <isCase,True,_>   => <vpnn.p1 ++ obj ! a, vpnn.p2, vpnn.p3, vpnn.p4> ; -- accPron < pron
            <isCase,False,_>  => <vpnn.p1, vpnn.p2 ++ obj ! a, vpnn.p3, vpnn.p4> ; -- < non-pron nominal
            <_,_,_>           => <vpnn.p1, vpnn.p2, vpnn.p3 ++ obj ! a, vpnn.p4> } --   or prepositional
      } ;

-- SS: implementation of some of the relevant Foc rules from Extra

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
          subj = mkSubject np vp.c1 ;
          cl = mkClause subj.s subj.a vp
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
      let subj = mkSubject cl.subj cl.c1
      in DisToCl subj.s subj.a cl ;


  oper -- extra operations for ExtraGer

    mkFoc : Str -> Cl -> Foc = \focus, cl ->
		lin Foc {s = \\m,t,a,p => focus ++ cl.s ! m ! t ! a ! p ! Inv} ;

    esSubj : CatGer.NP = lin NP {
      s = \\_,_ => "es" ;
      rc, ext = [] ;
      a = AgSgP3 Neutr ;
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
          verb  = vps.s  ! ord ! agr2vagr agr ! VPFinite m t a ;
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
