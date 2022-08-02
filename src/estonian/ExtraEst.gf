concrete ExtraEst of ExtraEstAbs = CatEst **
  open ResEst, MorphoEst, Coordination, Prelude, NounEst, StructuralEst, (R = ParamX), (P = ParadigmsEst) in {
  flags coding=utf8;
  lin
    GenNP np = {
      s,sp = \\_,_ => linNP (NPCase Gen) np ;
      isNum  = False ;
      isDef  = True ; --- "Jussin kolme autoa ovat" ; thus "...on" is missing
      isNeg = False
     } ;

    GenCN = caseCN Genitive ;     -- soome mees
    ComitCN = caseCN Comitative ; -- puudega mets
    ElatCN  = caseCN Elative ;  -- puust laud
    AbessCN = caseCN Abessive ; -- autota pere
    TerminCN = caseCN Terminative ; -- maani kleit

    GenIP ip = {s = \\_,_ => linIP (NPCase Gen) ip} ;

    GenRP num cn = {
      s = \\n,c => let k = npform2case num.n c in relPron ! NCase n Gen ++ cn.s ! NCase num.n k ;
      a = RNoAg
---      a = RAg (agrP3 num.n)
      } ;
  oper
    caseCN : CasePlus -> NPhrase -> CNoun -> CNoun = \c,np,cn -> cn ** {
      s = \\nf => appCompl True Pos (P.casePrep c) np ++ cn.s ! nf
      } ;

  lincat
    VPI   = LinVPI ;
    [VPI] = LinListVPI ;

  oper
    LinVPI     : Type = {s     : InfStem => Str} ;
    LinListVPI : Type = {s1,s2 : InfStem => Str} ;

    linVPI : InfForms -> LinVPI -> Str = \inf,vpi -> vpi.s ! inf.stem ;

    -- Version that uses InfStem
    infVPIF : NPForm -> Polarity -> Agr -> ResEst.VP -> InfStem -> Str = \sc,pol,agr,vp,if ->
      infVPAnt Simul sc pol agr vp {stem=if ; suf="a"} ;

  lin
    BaseVPI = twoTable InfStem ;
    ConsVPI = consrTable InfStem comma ;

    MkVPI vp = {s = \\i => infVPIF (NPCase Nom) Pos (agrP3 Sg) vp i} ;
    ConjVPI = conjunctDistrTable InfStem ;
    ComplVPIVV vv vpi =
      insertObj (\\_,_,_ => vpi.s ! vv.vi.stem) (predV vv) ;

  lincat
    VPS   = LinVPS ;
    [VPS] = LinListVPS ;
  oper
    LinVPS : Type = {
      s   : Agr  => Str ;
      sc  : NPForm ;  --- can be different for diff parts
      } ;
    LinListVPS : Type = {
      s1,s2 : Agr  => Str ;
      sc    : NPForm ;  --- take the first: minä osaan kutoa ja täytyy virkata
      } ;

    linVPS : Agr -> {s : Agr => Str} -> Str = \agr,vps -> vps.s ! agr ;

    -- This internal oper isn't used in any of the RGL linearisations, but can be useful for application grammars
    -- It produces a telegraphic style in past participle, 'võetud …' instead of 'on/oli võetud …'.
    -- It differs from PastPartAP in word order, and it also takes polarity.
    TelegraphicPastPartPassVPS : Pol -> ResEst.VP -> VPS = \p,vp ->
      let sentIsPos : Bool = case p.p of {
            Neg => False ;
            Pos => True } ;
          neg : Str = case p.p of {
            Neg => "ei" ;
            Pos => [] } ;
       in lin VPS {
            s = \\a => neg                            -- ei
                    ++ vp.v.s ! (PastPart Pass)       -- võetud
                    ++ vp.s2 ! sentIsPos ! p.p ! a    -- vereanalüüs
                    ++ vp.adv                         -- eile
                    ++ vp.p
                    ++ vp.ext ;
            sc = vp.sc
          };

  lin
    BaseVPS x y = twoTable Agr x y ** {sc = x.sc} ;
    ConsVPS x y = consrTable Agr comma x y ** {sc = x.sc} ;

    ConjVPS conj ss = conjunctDistrTable Agr conj ss ** {
      sc = ss.sc
      } ;

    MkVPS t p vp = { --  Temp -> Pol -> VP -> VPS ;
      s = \\a => let vps = mkVPForms vp.v ! VIFin t.t ! t.a ! p.p ! a
                 in
                 t.s ++ p.s ++
                 vps.fin ++ vps.inf ++
                 vp.s2 ! True ! p.p ! a ++
                 vp.adv ++
                 vp.ext ;
      sc = vp.sc ;
      } ;

    PredVPS np vps = { -- NP -> VPS -> S ;
      s = subjForm np vps.sc Pos ++ vps.s ! np.a
      } ;

   PassVPSlash vp = vp ; --passVP vp vp.c2 ;


   PassAgentVPSlash vp np = vp ;
 {-
      s = {s = vp.s.s ; h = vp.s.h ; p = vp.s.p ; sc = npform2subjcase vp.c2.c} ;
      s2 = \\b,p,a => linNP (NPCase Nom) np ++ vp.s2 ! b ! p ! a ;
      adv = vp.adv ;
      ext = vp.ext ;
      vptyp = vp.vptyp ;
      } ; -}

    AdvExistNP adv np =
      mkClause (\_ -> adv.s) np.a (insertObj
        (\\_,b,_ => linNP (NPCase Nom) np) (predV (verbOlema ** {sc = NPCase Nom}))) ;

    RelExistNP prep rp np = {
      s = \\t,ant,bo,ag =>
      let
        n = complNumAgr ag ;
        cl = mkClause
          (\_ -> appCompl True Pos prep (rp2np n rp))
          np.a
          (insertObj
            (\\_,b,_ => linNP (NPCase Nom) np)
            (predV (verbOlema ** {sc = NPCase Nom}))) ;
      in
        cl.s ! t ! ant ! bo ;
      c = NPCase Nom
      } ;

    AdvPredNP  adv v np =
      mkClause (\_ -> adv.s) np.a (insertObj
        (\\_,b,_ => linNP (NPCase Nom) np) (predV v)) ;

    ICompExistNP adv np =
      let subj : Polarity -> Str = \_ -> adv.s ! np.a ;
          pred : ResEst.VP = insertObj
                               (\\_,b,_ => linNP (NPCase Nom) np)
                               (predV (verbOlema ** {sc = NPCase Nom})) ;
       in mkClause subj np.a pred ;


    IAdvPredNP iadv v np =
      let subj : Polarity -> Str = \_ -> iadv.s ;
          pred : ResEst.VP = insertObj
                               (\\_,b,_ => linNP v.sc np)
                               (predV v) ;
       in mkClause subj np.a pred ;


--    i_implicPron = mkPronoun [] "minun" "minua" "minuna" "minuun" Sg P1 ;
    whatPart_IP = emptyIP ** {
      s = table {
        NPCase Nom | NPAcc => "mida" ;
        c => whatSg_IP.s ! c
        } ;
      n = Sg
    } ;

    PartCN cn =
      let
        acn = DetCN (DetQuant IndefArt NumSg) cn
      in acn ** {
        s = table {
          NPCase Nom | NPAcc => acn.s ! NPCase ResEst.Part ;
          c => acn.s ! c
          } ;
        isPron = False ; isNeg = False
        } ;

    --The reflexive possessive "oma"
    --for "ta näeb oma koera" instead of *"tema koera"
    OmaPoss = {s,sp = \\_,_ => "oma" ; isDef,isNeg,isNum = False} ;

    ma_Pron = shortPronoun "ma" "mu" "mind" "minu" Sg P1 ;
    sa_Pron = shortPronoun "sa" "su" "sind" "sinu" Sg P2;
    ta_Pron = shortPronoun "ta" "ta" "teda" "tema" Sg P3 ;
    me_Pron =
    {s = table {
        NPCase Nom => "me" ;
        n => (we_Pron.s) ! n
        } ;
     a = Ag Pl P1 } ;

    te_Pron =
    {s = table {
        NPCase Nom => "te" ;
        n => (youPl_Pron.s) ! n
        } ;
     a = Ag Pl P2 } ;

    nad_Pron =
    {s = table {
        NPCase Nom => "nad" ;
        n => (they_Pron.s) ! n
        } ;
     a = Ag Pl P3 } ;

---- copied from VerbEst.CompAP, should be shared
    ICompAP ap = {
      s = \\agr =>
          let
            n = complNumAgr agr ;
            c = case n of {
              Sg => Nom ;  -- Fin (Nom): minä olen iso ; te olette iso
              Pl => Nom    -- Fin (Part): me olemme isoja ; te olette isoja
              }            --- definiteness of NP ?
          in "kui" ++ ap.s ! False ! (NCase n c)
      } ;

    IAdvAdv adv = {s = "kui" ++ adv.s} ;

    ProDrop p = {
      s = table {NPCase (Nom | Gen) => [] ; c => p.s ! c} ;
          ---- drop Gen only works in adjectival position
      a = p.a
      } ;

    -- : Pron -> Quant ;
    ProDropPoss p = {
      s = \\_,_ => "oma" ;
      sp = \\_,_ => p.s ! NPCase Gen ;
      isNum = False ;
      isDef = True ;
      isNeg = False
      } ;

  lincat
    ClPlus, ClPlusObj, ClPlusAdv = ClausePlus ;
    Part = {s : Str} ;

  lin
    S_SVO part t p clp =
      let
         cl = clp.s ! t.t ! t.a ! p.p ;
         pa = part.s ----
      in
      {s = t.s ++ p.s ++ cl.subj ++ pa ++ cl.fin ++ cl.inf ++ cl.compl ++ cl.adv ++ cl.ext} ;

    S_OSV part t p clp =
      let
         cl = clp.s ! t.t ! t.a ! p.p ;
         pa = part.s ----
      in
      {s = t.s ++ p.s ++ cl.compl ++ pa ++ cl.subj ++ cl.fin ++ cl.inf ++ cl.adv ++ cl.ext} ;
    S_VSO part t p clp =
      let
         cl = clp.s ! t.t ! t.a ! p.p ;
         pa = part.s
      in
      {s = t.s ++ p.s ++ cl.fin ++ pa ++ cl.subj ++ cl.inf ++ cl.compl ++ cl.adv ++ cl.ext} ;
    S_ASV part t p clp =
      let
         cl = clp.s ! t.t ! t.a ! p.p ;
         pa = part.s
      in
      {s = t.s ++ p.s ++ cl.adv ++ pa ++ cl.subj ++ cl.fin ++ cl.inf ++ cl.compl ++ cl.ext} ;

    S_OVS part t p clp =
      let
         cl = clp.s ! t.t ! t.a ! p.p ;
         pa = part.s ----
      in
      {s = t.s ++ p.s ++ cl.compl ++ pa ++ cl.fin ++ cl.inf ++ cl.subj ++ cl.adv ++ cl.ext} ;

    PredClPlus np vp = mkClausePlus (subjForm np vp.sc) np.a vp ;
    PredClPlusFocSubj np vp = insertKinClausePlus 0 (mkClausePlus (subjForm np vp.sc) np.a vp) ;
    PredClPlusFocVerb np vp = insertKinClausePlus 1 (mkClausePlus (subjForm np vp.sc) np.a vp) ;
    PredClPlusObj  np vps obj =
      insertObjClausePlus 0 False (\\b => appCompl True b vps.c2 obj) (mkClausePlus (subjForm np vps.sc) np.a vps) ;
    PredClPlusFocObj  np vps obj =
      insertObjClausePlus 0 True (\\b => appCompl True b vps.c2 obj) (mkClausePlus (subjForm np vps.sc) np.a vps) ;
    PredClPlusAdv  np vp  adv =
      insertObjClausePlus 1 False (\\_ => adv.s) (mkClausePlus (subjForm np vp.sc) np.a vp) ;
    PredClPlusFocAdv  np vp  adv =
      insertObjClausePlus 1 True (\\_ => adv.s) (mkClausePlus (subjForm np vp.sc) np.a vp) ;

    ClPlusWithObj c = c ;
    ClPlusWithAdv c = c ;

    gi_Part = ss "gi" | ss "ki" ;

}
