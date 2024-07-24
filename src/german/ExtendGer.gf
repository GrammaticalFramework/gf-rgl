--# -path=alltenses:../common:../abstract

concrete ExtendGer of Extend =
  CatGer ** ExtendFunctor
  - [ -- remove the default implementations of:
      GenNP, GenRP, EmptyRelSlash,
      VPS, ListVPS, MkVPS, BaseVPS, ConsVPS, ConjVPS, PredVPS,
      VPI, ListVPI, MkVPI, BaseVPI, ConsVPI, ConjVPI, ComplVPIVV,
      ICompAP, IAdvAdv, CompIQuant, PrepCN,
      PastPartAP, PastPartAgentAP,
      PassVPSlash, PassAgentVPSlash,
      AdvIsNP,
      RNP, RNPList, Base_rr_RNP, Base_nr_RNP, Base_rn_RNP, Cons_rr_RNP, Cons_nr_RNP, Conj_RNP,
      ReflRNP, ReflPron, ReflPoss, PredetRNP, AdvRNP, ReflA2RNP, PossPronRNP,
      CompoundN, DetNPMasc, DetNPFem, UseDAP, UseDAPMasc, UseDAPFem,
      CardCNCard,
      InOrderToVP
    ]
  with
    (Grammar = GrammarGer) **
  open
    GrammarGer,
    ResGer,
    Coordination,
    Prelude,
    (P = ParadigmsGer) in {

  lin
    GenNP np =
      let tab : GenNum => Case => Str =
            \\gn,c => np.s ! False ! Gen ++ np.ext ++ np.rc
      in {s = \\_ => tab ;
          sp = tab ;
          a = Strong ;
          isDefArt = False ;
          delCardOne = False
      } ;

    GenRP nu cn = {
      s = \\gn,c => relPron ! gn ! Gen ++ cn.s ! Weak ! nu.n ! c ;
      a = RAg nu.n P3
      } ;



    EmptyRelSlash slash = {
      s = \\m,t,a,p,gn =>
        appPrep slash.c2 (relPron ! gn) ++ slash.s ! m ! t ! a ! p ! Sub ;
      c = slash.c2.c
      } ;


  lincat
    VPI   = {s : Bool => Str} ;
    [VPI] = {s1,s2 : Bool => Str} ;
    VPS   = {s : Order => Agr => {verb, compl : Str}} ;
    [VPS] = {s : Order => Agr => {s1, s2, s3 : Str}} ; -- liebe, (ich) dich, (und) bin glücklich

  lin
    BaseVPI = twoTable Bool ;
    ConsVPI = consrTable Bool comma ;

    BaseVPS v w = {
      s = \\ord, agr =>
        let
	  vs = v.s ! ord ! agr ;
	  ws = w.s ! ord ! agr ;
	in {
	  s1 = vs.verb ;
	  s2 = vs.compl ;
	  s3 = case ord of {
	    Sub => ws.compl ++ ws.verb ;
	    _ => ws.verb ++ ws.compl
	    }
	  }
       } ;
	  
    ConsVPS v vv = {
      s = \\ord, agr =>
        let
	  vs = v.s ! ord ! agr ;
	  vvs = vv.s ! ord ! agr ;
	in {
	  s1 = vs.verb ;
	  s2 = vs.compl ++ comma ++ vvs.s1 ++ vvs.s2 ;
	  s3 = vvs.s3
	  }
      } ;

    ConjVPS conj vv = {
      s = \\ord, agr =>
         let
           vvs = vv.s ! ord ! agr
         in {
	   verb = vvs.s1 ;
	   compl = conj.s1 ++ vvs.s2 ++ conj.s2 ++ vvs.s3
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
          verb  = vps.s  ! ord ! agr2vagr agr ! VPFinite m t a ;
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
             <Fut|Cond,Anter,True> => [] ;                                --# notpresent
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
	--- AR 22/7/2024 as the subject comes to a wrong place in PredVPS Inv
	{verb = verb.fin ; compl = compl ++ infCompl ++ pred.inf ++ extra} 
	{-
        case o of {
	  Main => verb.fin ++ compl ++ infCompl ++ pred.inf ++ extra ;
	  Inv  => verb.fin ++ subj ++ compl ++ infCompl ++ pred.inf ++ extra ;
	  Subj =>             subj ++ compl ++   pred.infComplfin   ++ extra
        }
	-}
    } ;


    PredVPS np vps =
      let
        subj = np.s ! False ! Nom ++ bigNP np ;
        agr  = np.a ;
      in {
        s = \\o =>
          let verb = vps.s ! o ! agr
          in case o of {
            Main => subj ++ verb.verb ++ verb.compl ;
            Inv  => verb.verb ++ subj ++ verb.compl ;   -- älskar jag henne och sover
            Sub  => subj ++ verb.compl ++ verb.verb     --- not quite correct in ConjVPS
            }
        } ;

-- existentials that work in the absence of Cl
    
    MkVPI vp = {s = \\b => useInfVP b vp} ;

    ConjVPI = conjunctDistrTable Bool ;

    ComplVPIVV v vpi =
      insertInf {inpl = <\\_ => [], (vpi.s ! v.isAux)> ; extr = \\_ => []}  -- HL 3/22
        (predVGen v.isAux v) ;

-- the same for VPSlash, taking a complement with shared V2 verbs

-- Conjunction of copula complements

-- Conjunction of imperatives

    ICompAP ap = {
      s = \\_ => "wie" ++ ap.s ! APred ;
      ext = ap.c.p1 ++ ap.c.p2 ++ ap.ext
      } ;

    IAdvAdv adv = {s = "wie" ++ adv.s} ;

    CompIQuant iq = {
      s = \\a => iq.s ! (gennum (genderAgr a) (numberAgr a))! Nom ;
      ext = ""
      } ;

    PrepCN prep cn = {
      s = prep.s ! GPl ++ cn.s ! Strong ! Sg ! prep.c ++ cn.adv ++ cn.rc ! Sg ++ cn.ext} ;

  -- fronted/focal constructions, only for main clauses

  -- participle constructions

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
                  ++ vp.c2.s ! GPl                     -- junk if not TV
                  ++ vp.ext ++ (vp.inf.extr ! a) ++ vp.s.s ! VPastPart af ;
      s2 = \\_ => [] ;
      isPre = True ;
      c = <[],[]> ;
      ext = []
      } ;

-- this is a generalization of Verb.PassV2 and should replace it in the future.

    PassVPSlash vp =
      insertObj (\\_ => (PastPartAP vp).s ! APred) (predV werdenPass) **
          { c1 = subjPrep vp.c2 } ;
    -- this also gives "mit dir wird gerechnet" ;
    -- the alternative linearisation ("es wird mit dir gerechnet") is not implemented

    PassAgentVPSlash vp np = ---- "von" here, "durch" in StructuralGer
      insertObj (\\_ => (PastPartAgentAP (lin VPSlash vp) (lin NP np)).s ! APred) (predV werdenPass) ;

-- publishing of the document

-- counterpart to ProgrVP, for VPSlash

-- construct VPSlash from A2 and N2

-- existential for mathematics

-- existentials with a/no variation

-- generalisation of existential, with adverb as an argument

    AdvIsNP adv np = let comp : Agr => Str = \\agr => np.s ! False ! Nom ++ np.rc ++ np.ext in
      mkClause adv.s np.a (insertObj comp (predV sein_V)) ; -- HL 12/2023, prelim, wrong order

-- infinitive for purpose AR 21/8/2013

-- object S without "that"

-- front the extraposed part

-- proper structure of "it is AP to VP"

-- to use an AP as CN or NP without CN

-- infinitive complement for IAdv

-- alternative to EmbedQS. For English, EmbedQS happens to work,

-- Reflexive noun phrases -- (HL 5/2022: improved and completed, RNPList added)

  lincat
    RNP = {s : Agr => Case => Str ; rc,ext : Str ; isPron : Bool} ;
    RNPList = {s1,s2 : Agr => Case => Str} ;

  linref
    RNP = \rnp -> rnp.s ! AgSgP3 Masc ! Acc ++ rnp.ext ++ rnp.rc ;

  lin
    ReflRNP vps rnp =
      insertObjReflNP (lin RNP rnp) vps ;

    ReflPron = { -- with personal pronoun nominative
      s = ResGer.reflPron ; rc,ext = [] ; isPron = True } ;

    -- To distinguish personal pronoun from reflexive pronoun:
    --   du kennst mich vs. ich kenne mich selbst
    --   er kennt ihn   vs. er kennt sich (selbst)
    --   sie kennen sich (selbst) =/= sie kennen einander
    -- we might define ReflPron by the stronger
    -- oper
    --   reflPronSelf : Agr => Case => Str =
    --   \\a => \\c => reflPron ! a ! c ++ "selbst" ;

  lin
    ReflPoss num cn =
      {s = \\a,c => let adjf = case num.n of {Sg => Strong ; Pl => Weak} -- Duden 477, HL 5/2022
         in possPron a num.n cn.g c
            ++ num.s ! AMod (gennum cn.g num.n) c -- HL 5/2022: meine wenigstens 3 cn,
            ++ cn.s ! adjfCase adjf c ! num.n ! c --       not: wenigstens 3 meine cn
            ++ cn.adv ;
       ext = cn.ext ; rc = cn.rc ! num.n ;
       isPron = False} ;

    -- To distinguish possessive pronoun from reflexive possessive pronoun:
    --   du kennst meine Fehler vs. ich kenne meine eigenen Fehler
    --   er|sie|es kennt seine|ihre Fehler  vs. er|sie|es kennt seine|ihre|seine eigenen Fehler
    -- we might define ReflPoss by the stronger
    -- oper
    --   reflPossPron : Agr -> Number -> Gender -> Case -> Str =
    --   let eigen = adjForms "eigen" "eigen" in
    --     \a,n,g,c -> possPron a n g c ++ (eigen ! (AMod (gennum g n) c)) ;

    PredetRNP pred rnp = rnp ** {                        -- HL 5/2022
      s = \\a,c => let n : Number = case pred.a of {PAg n => n ; _ => numberAgr a} ;
                       g : Gender = genderAgr a ;
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
        compl = appPrep adj.c2 (rnp.s ! agrP3 Sg) ; -- we use a fixed agreement
      in {
        s = adj.s ! Posit ;
        s2 = \\_ => [] ;
        isPre = True ;
        c = case adj.c2.t of {isCase => <compl, []> ; _ => <[], compl>} ;
        ext = rnp.ext ++ rnp.rc
      } ;

    PossPronRNP pron num cn rnp =
      GrammarGer.DetCN (GrammarGer.DetQuant (GrammarGer.PossPron pron) num)
      (GrammarGer.PossNP cn (lin NP {s = \\_,c => rnp.s ! pron.a ! c ;
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

-- reflexive possessive on its own right, like in Swedish, Czech, Slovak

--- from Extensions

    CompoundN a x =
      let s = a.co in lin N {
        s  = \\n,c => s ++ Predef.BIND ++ x.uncap.s ! n ! c ;
        co = s ++ Predef.BIND ++ x.uncap.co ;
        uncap = {
          s  = \\n,c => a.uncap.co  ++ Predef.BIND ++ x.uncap.s ! n ! c ;
          co = a.uncap.co  ++ Predef.BIND ++ x.uncap.co ;
          } ;
        g = x.g
      } ;

-- very language-specific things

-- Romance

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

-- German

-- UseDAP replaces DetNP from the RGL which is more limited.

    UseDAP det = {
      s = \\b,c => det.sp ! Neutr ! c ;
      a = agrP3 det.n ;
      w = case det.isDef of { True => WLight ; _ => WHeavy } ;
      rc, ext = []
      } ;

    UseDAPMasc det = {
      s = \\b,c => det.sp ! Masc ! c ;
      a = agrP3 det.n ;
      w = WLight ;
      rc, ext = []
      } ;

    UseDAPFem det = {
      s = \\b,c => det.sp ! Fem ! c ;
      a = agrP3 det.n ;
      w = WLight ;
      rc, ext = []
      } ;

    CardCNCard card cn = {
      s = let det = (Grammar.DetQuant Grammar.IndefArt (Grammar.NumCard card)) ;
              np = (Grammar.DetCN det cn).s ! False
        in table{APred => np ! Nom ; AMod gn c => np ! c} ; -- HL 12/2023
      n = Pl
      } ;


    InOrderToVP vp = {s = "um" ++ useInfVP False vp} ;

  oper
    insertObjReflNP : RNP -> ResGer.VPSlash -> ResGer.VP = -- HL 5/2022
      \rnp,vp -> insertObjRNP rnp vp.c2 vp ;

    insertObjRNP : RNP -> Preposition -> ResGer.VPSlash -> ResGer.VP = -- HL 5/2022
      \rnp,prep,vp ->                                           -- generalize ResGer.insertObjRefl
      let
        obj : Agr => Str = \\a => prep.s ! GPl ++ rnp.s ! a ! prep.c ++ rnp.ext ++ rnp.rc
      in vp ** {
        nn = \\a =>
          let vpnn = vp.nn ! a in
          case <prep.t, rnp.isPron, prep.c> of {   -- consider non-pron rnp as light, add to vpnn.p2
            <isCase,True,Acc> => <obj ! a ++ vpnn.p1, vpnn.p2, vpnn.p3, vpnn.p4> ; -- pronoun switch:
            <isCase,True,_>   => <vpnn.p1 ++ obj ! a, vpnn.p2, vpnn.p3, vpnn.p4> ; -- accPron < pron
            <isCase,False,_>  => <vpnn.p1, vpnn.p2 ++ obj ! a, vpnn.p3, vpnn.p4> ; -- < non-pron nominal
            <_,_,_>           => <vpnn.p1, vpnn.p2, vpnn.p3 ++ obj ! a, vpnn.p4> } --   or prepositional
      } ;

}
