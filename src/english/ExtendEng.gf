--# -path=.:../common:../abstract

concrete ExtendEng of Extend =
  CatEng ** ExtendFunctor -
  [
    VPS, ListVPS, VPI, ListVPI, VPS2, ListVPS2, VPI2, ListVPI2, RNP, RNPList,
    AdAdV, AdjAsCN, AdjAsNP, ApposNP, AdvIsNP,
    MkVPS, BaseVPS, ConsVPS, ConjVPS, PredVPS, QuestVPS, SQuestVPS, RelVPS,
    MkVPI, BaseVPI, ConsVPI, ConjVPI, ComplVPIVV,
    MkVPS2, BaseVPS2, ConsVPS2, ConjVPS2, ComplVPS2, ReflVPS2,
    MkVPI2, BaseVPI2, ConsVPI2, ConjVPI2, ComplVPI2,
    Base_nr_RNP, Base_rn_RNP, Base_rr_RNP, ByVP, CompBareCN,
    CompIQuant, CompQS, CompS, CompVP, ComplBareVS, ComplGenVV, ComplSlashPartLast, ComplVPSVV, CompoundAP,
    CompoundN, ConjRNP, Cons_nr_RNP, Cons_rr_RNP, DetNPMasc, DetNPFem, EmbedPresPart, EmptyRelSlash,
    ExistsNP, ExistCN, ExistMassCN, ExistPluralCN,
    FocusAP, FocusAdV, FocusAdv, FocusObj, GenIP, GenModIP, GenModNP, GenNP, GenRP,
    GerundAdv, GerundCN, GerundNP, IAdvAdv, ICompAP, InOrderToVP, NominalizeVPSlashNP,
    PassAgentVPSlash, PassVPSlash, ProgrVPSlash, PastPartAP, PastPartAgentAP, PositAdVAdj,  PredVPSVV, PredetRNP, PrepCN,
    EmbedSSlash, PredIAdvVP, PresPartAP, PurposeVP, ReflPoss, ReflPron, ReflRNP, SlashBareV2S, SlashV2V, StrandQuestSlash, StrandRelSlash,
    UncontractedNeg, UttAccIP, UttAccNP, UttAdV, UttDatIP, UttDatNP, UttVPShort, WithoutVP, A2VPSlash, N2VPSlash,
    CardCNCard, ProDrop
   ]
  with
    (Grammar = GrammarEng) **

  open
    GrammarEng,
    ResEng,
    Coordination,
    Prelude,
    MorphoEng,
    ParadigmsEng in {

  lin
    ProDrop pro = pro ** {
      s = \\_ => []
    } ;

    GenNP np = {s = \\_,_ => np.s ! npGen ; sp = \\_,_,_,_ => np.s ! npGen ; isDef = True} ;
    GenIP ip = {s = \\_ => ip.s ! NCase Gen} ;
    GenRP nu cn = {
      s = \\c => "whose" ++ nu.s ! False ! Nom ++
                 case c of {
                   RC _ (NCase Gen) => cn.s ! nu.n ! Gen ;
                   _ => cn.s ! nu.n ! Nom
                   } ;
      a = RAg (agrP3 nu.n)
      } ;

    GenModNP num np cn = DetCN (DetQuant (GenNP (lin NP np)) num) cn ;
    GenModIP num ip cn = IdetCN (IdetQuant (GenIP (lin IP ip)) num) cn ;

    StrandQuestSlash ip slash =
      {s = \\t,a,b,q =>
         (mkQuestion (ss (ip.s ! NPAcc)) slash).s ! t ! a ! b ! q ++ slash.c2
      };
    StrandRelSlash rp slash = {
      s = \\t,a,p,ag =>
        rp.s ! RC (fromAgr ag).g NPAcc ++ slash.s ! t ! a ! p ! oDir ++ slash.c2 ;
      c = NPAcc
      } ;
    EmptyRelSlash slash = {
      s = \\t,a,p,_ => slash.s ! t ! a ! p ! oDir ++ slash.c2 ;
      c = NPAcc
      } ;

    DetNPMasc det = {
      s = det.sp ! Masc ! False ;
      a = agrgP3 det.n Masc
      } ;

    DetNPFem det = {
      s = det.sp ! Fem ! False ;
      a = agrgP3 det.n Fem
      } ;

  lincat
    VPS   = {s : Order => Agr => {fin, inf : Str}} ;
    [VPS] = {s1,s2 : Order => Agr => Str ; fin : Order => Agr => Str} ;
    VPI   = {s : VVType => Agr => Str} ;
    [VPI] = {s1,s2 : VVType => Agr => Str} ;

  lin
    BaseVPS x y =
      let baseX : OneFinVPS = baseVPS x ;
          baseY : OneFinVPS = baseVPS y ;
       in twoTable2 Order Agr baseX baseY ** {fin = baseX.fin} ;

    ConsVPS x xs =
      let baseX : OneFinVPS = baseVPS x ;
       in consrTable2 Order Agr comma baseX xs ** {fin = baseX.fin} ; -- keep only the first fin
  oper
    {- IL 03/2021 for VPS conjunction: separate the first finite verb for questions
           [do]:fin (you) [eat bread and sleep?]:inf
           (you) [  ]:fin [eat bread and sleep]:inf
       The fin field becomes empty for ODir, but no problem: order is just subj++fin++inf.
       Weird results for CompNP/AP/…, but coordination of those already covered in RGL. -}
    OneFinVPS : Type = {s : Order => Agr => Str ; fin : Order => Agr => Str} ;
    baseVPS : VPS -> OneFinVPS = \vps -> {
      s = \\o,a => let vp = vps.s ! o ! a in
        case o of {
          OQuest => vp.inf ;
          ODir _ => vp.fin ++ vp.inf
        } ;
      fin = \\o,a => let vp = vps.s ! o ! a in
        case o of {
          OQuest => vp.fin ;
          ODir _ => []
        }
      } ;

  lin
    BaseVPI = twoTable2 VVType Agr ;
    ConsVPI = consrTable2 VVType Agr comma ;

    MkVPS t p vp = mkVPS (lin Temp t) (lin Pol p) (lin VP vp) ;
    ConjVPS c xs = {
      s = \\o,a => {
        fin = xs.fin ! o ! a ;
        inf = c.s1 ++ xs.s1 ! o ! a ++ c.s2 ++ xs.s2 ! o ! a}
      } ;
    PredVPS np vps = let vp = vps.s ! oDir ! np.a in {
      s = np.s ! npNom ++ vp.fin ++ vp.inf
      } ;
    SQuestVPS np vps = let
      vp = vps.s ! OQuest ! np.a ;
      vpindir = vps.s ! oDir ! np.a in {
      s = table {
        QDir => vp.fin ++ np.s ! npNom ++ vp.inf ;
        QIndir => "if" ++ np.s ! npNom ++ vpindir.fin ++ vpindir.inf}
      } ;
    QuestVPS ip vps = let vp = vps.s ! oDir ! toAgr ip.n P3 Neutr  in {
      s = \\q => ip.s ! npNom ++ vp.fin ++ vp.inf
      } ;
    RelVPS rp vps = {
      s = \\agr => let vp = vps.s ! oDir ! agr in
           rp.s ! RC (fromAgr agr).g npNom ++ vp.fin ++ vp.inf ;
      c = npNom ;
      } ;


    MkVPI vp = mkVPI (lin VP vp) ;
    ConjVPI c xs = conjunctDistrTable2 VVType Agr c xs ;
    ComplVPIVV vv vpi = insertObj (\\a => vpi.s ! vv.typ ! a) (predVV vv) ;


-------- two-place verb conjunction

  lincat
    VPS2   = {s : Order => Agr => {fin,inf : Str} ; c2 : Str} ;
    [VPS2] = {
      fin : Order => Agr => Str ;   -- Q: do         ; DIR:
      s1,s2 : Order => Agr => Str ; -- Q: eat, drink ; DIR: eat, drink
      c2 : Str} ;
    VPI2   = {s : VVType => Agr => Str ; c2 : Str} ;
    [VPI2] = {s1,s2 : VVType => Agr => Str ; c2 : Str} ;

  lin
    MkVPS2 t p vpsl = mkVPS (lin Temp t) (lin Pol p) (lin VP vpsl) ** {c2 = vpsl.c2} ;
    MkVPI2 vpsl = mkVPI (lin VP vpsl) ** {c2 = vpsl.c2} ;

    BaseVPS2 x y = BaseVPS x y ** {c2 = y.c2} ; ---- just remembering the prep of the latter verb
    ConsVPS2 x xs = ConsVPS x xs ** {c2 = xs.c2} ;

    BaseVPI2 x y = twoTable2 VVType Agr x y ** {c2 = y.c2} ; ---- just remembering the prep of the latter verb
    ConsVPI2 x xs = consrTable2 VVType Agr comma x xs ** {c2 = xs.c2} ;


    ConjVPS2 c xs = ConjVPS c xs ** {c2 = xs.c2} ;
    ConjVPI2 c xs = conjunctDistrTable2 VVType Agr c xs ** {c2 = xs.c2} ;


    ComplVPS2 vps2 np = {
        s = \\o,a => let vps2s = vps2.s ! o ! a in
            vps2s ** {inf = vps2s.inf ++ vps2.c2 ++ np.s ! NPAcc} -- keep fin, add object to inf
        } ;
    ComplVPI2 vpi2 np = {
        s = \\t,a => vpi2.s ! t ! a ++ vpi2.c2 ++  np.s ! NPAcc
        } ;
    ReflVPS2 vps2 rnp = {
        s = \\o,a => let vps2s = vps2.s ! o ! a in
            vps2s ** {inf = vps2s.inf ++ vps2.c2 ++ rnp.s ! a}
        } ;
  oper
    mkVPS : Temp -> Pol -> VP -> VPS = \t,p,vp -> lin VPS {
      s = \\o,a =>
        let
          verb = mkVerbForms a vp ! t.t ! t.a ! p.p ! o ! a ; -- choice of Order determines aux or not
          compl = vp.s2 ! a ++ vp.ext
        in {fin = verb.aux ++ t.s ++ p.s ;
            inf = verb.adv ++ vp.ad ! a ++ verb.fin ++ verb.inf ++ vp.p ++ compl} ;
      } ;

    linVPS : Agr -> {s : Order => Agr => {fin,inf : Str}} -> Str = \agr,vps -> let vpss = vps.s ! ODir True ! agr in vpss.fin ++ vpss.inf ;

    mkVPI : VP -> VPI = \vp -> lin VPI {
      s = table {
            VVAux      => \\a =>         vp.ad ! a ++ vp.inf ++ vp.p ++ vp.s2 ! a ;
            VVInf      => \\a => "to" ++ vp.ad ! a ++ vp.inf ++ vp.p ++ vp.s2 ! a ;
            VVPresPart => \\a =>         vp.ad ! a ++ vp.prp ++ vp.p ++ vp.s2 ! a
            }
      } ;

linref
  VPS = linVPS (agrP3 Sg) ;
  VPS2 = \vps -> linVPS (agrP3 Sg) vps ++ vps.c2 ;

-- Conjunction of copula complements

lincat [Comp] = {s1,s2 : Agr => Str} ;
lin BaseComp x y = twoTable Agr x y ;
    ConsComp xs x = consrTable Agr comma xs x ;
    ConjComp conj ss = conjunctDistrTable Agr conj ss ;

-- Conjunction of imperatives

lincat ListImp = {s1,s2 : CPolarity => ImpForm => Str} ;
lin BaseImp = twoTable2 CPolarity ImpForm ;
    ConsImp = consrTable2 CPolarity ImpForm comma ;
    ConjImp conj ss = conjunctDistrTable2 CPolarity ImpForm conj ss ;

-----

  lin
    ICompAP ap = {s = "how" ++ ap.s ! agrP3 Sg} ; ---- IComp should have agr!

    IAdvAdv adv = {s = "how" ++ adv.s} ;

    PresPartAP vp = {
      s = \\a => vp.ad ! a ++ vp.prp ++ vp.p ++ vp.s2 ! a ++ vp.ext ;
      isPre = vp.isSimple                 -- depends on whether there are complements
      } ;

    EmbedPresPart vp = {s = infVP VVPresPart vp False Simul CPos (agrP3 Sg)} ;

   PastPartAP vp = {
      s = \\a => vp.ad ! a ++ vp.ptp ++ vp.p ++ vp.c2 ++ vp.s2 ! a ++ vp.ext ;
      isPre = vp.isSimple                 -- depends on whether there are complements
      } ;
   PastPartAgentAP vp np = {
      s = \\a => vp.ad ! a ++ vp.ptp ++ vp.p ++ vp.c2 ++ vp.s2 ! a ++ "by" ++ np.s ! NPAcc ++ vp.ext ;
      isPre = False
      } ;

   GerundCN vp = {
     s = \\n,c => vp.ad ! AgP3Sg Neutr ++ vp.prp ++
                  case <n,c> of {
                    <Sg,Nom> => "" ;
                    <Sg,Gen> => Predef.BIND ++ "'s" ;
                    <Pl,Nom> => Predef.BIND ++ "s" ;
                    <Pl,Gen> => Predef.BIND ++ "s'"
                    } ++
                  vp.p ++ vp.s2 ! AgP3Sg Neutr ++ vp.ext ;
     g = Neutr
     } ;

   GerundNP vp =
     let a = AgP3Sg Neutr ---- agr
     in
     {s = \\_ => vp.ad ! a ++ vp.prp ++ vp.p ++ vp.s2 ! a ++ vp.ext ; a = a} ;

   GerundAdv vp =
     let a = AgP3Sg Neutr
     in
     {s = vp.ad ! a ++ vp.prp ++ vp.p ++ vp.s2 ! a ++ vp.ext} ;

   WithoutVP vp = {s = "without" ++ (GerundAdv (lin VP vp)).s} ;

   InOrderToVP vp = {s = ("in order" | []) ++ infVP VVInf vp False Simul CPos (AgP3Sg Neutr)} ;

   PurposeVP vp = {s = infVP VVInf vp False Simul CPos (agrP3 Sg)} ; --- agr

   ByVP vp = {s = "by" ++ (GerundAdv (lin VP vp)).s} ;

   PredIAdvVP iadv vp = {s = \\t,a,p,q => iadv.s ++ infVP VVInf vp False Simul CPos (agrP3 Sg)} ;

   EmbedSSlash s = {s = "what" ++ s.s ++ s.c2} ;

   NominalizeVPSlashNP vpslash np =
     let vp : ResEng.VP = insertObjPre (\\_ => vpslash.c2 ++ np.s ! NPAcc) vpslash ;
         a = AgP3Sg Neutr
     in
     lin NP {s = \\_ => vp.ad ! a ++ vp.prp ++ vp.s2 ! a ; a = a} ;


  oper passVPSlash : VPSlash -> Str -> ResEng.VP =
   \vps,ag ->
    let
      be = predAux auxBe ;
      ppt = vps.ptp
    in be ** {
        p = [] ;
        ad = \\_ => [] ;
        s2 = \\a => vps.ad ! a ++ ppt ++ vps.p  ++ vps.s2 ! a ++ ag ++ vps.c2 ; ---- place of agent
        isSimple = False ;
        ext = vps.ext
    } ;

  lin
    PassVPSlash vps = passVPSlash (lin VPS vps) [] ;
    PassAgentVPSlash vps np = passVPSlash (lin VPS vps) ("by" ++ np.s ! NPAcc) ;
    ProgrVPSlash vp = insertObjc (\\a => vp.ad ! a ++ vp.prp ++ vp.p ++ vp.s2 ! a)
      (predAux auxBe ** {c2 = vp.c2; gapInMiddle = vp.gapInMiddle; missingAdv = vp.missingAdv});

    N2VPSlash n2 =
      let prep : Prep = mkPrep n2.c2 ;
          dummyVPS : VPSlash = SlashV2a (mkV2 (mkV "dummy") prep);
      in dummyVPS **  -- has necessary fields for VPSlash, and c2 from the N2
           UseComp (CompCN (UseN2 n2)) ; -- has all the right fields except for c2

    A2VPSlash a2 =
      let prep : Prep = mkPrep a2.c2 ;
          dummyVPS : VPSlash = SlashV2a (mkV2 (mkV "dummy") prep) ;
      in dummyVPS **  -- has necessary fields for VPSlash, and c2 from the A2
           UseComp (CompAP (UseA2 a2)) ; -- has all the right fields except for c2


   --- AR 7/3/2013
   ComplSlashPartLast vps np = case vps.gapInMiddle of {
     _  => insertObjPartLast (\\_ => vps.c2 ++ np.s ! NPAcc) vps  ---
     } ;

   --- AR 22/5/2013
   ExistsNP np =
      mkClause "there" (agrP3 (fromAgr np.a).n)
        (insertObj (\\_ => np.s ! NPAcc) (predV (regV "exist"))) ;

   ExistCN cn =
      let
         pos = ExistNP (DetCN (DetQuant IndefArt NumSg) cn) ;
         neg = ExistNP (DetCN (DetQuant no_Quant NumSg) cn) ;
      in posNegClause pos neg ;
   ExistMassCN cn =
      let
         pos = ExistNP (MassNP cn) ;
         neg = ExistNP (DetCN (DetQuant no_Quant NumSg) cn) ;
      in posNegClause pos neg ;
   ExistPluralCN cn =
      let
         pos = ExistNP (DetCN (DetQuant IndefArt NumPl) cn) ;
         neg = ExistNP (DetCN (DetQuant no_Quant NumPl) cn) ;
      in posNegClause pos neg ;


   ComplBareVS  v s = insertExtra s.s (predV v) ;
   SlashBareV2S v s = insertExtrac s.s (predVc v) ;

  CompoundN noun cn = {
    s = variants {\\n,c => noun.s ! Sg ! Nom ++                    cn.s ! n ! c ;
                  \\n,c => noun.s ! Sg ! Nom ++ BIND++"-"++BIND ++ cn.s ! n ! c} ;
    g = cn.g
  } ;

  CompoundAP noun adj = {
    s = variants {\\_ => noun.s ! Sg ! Nom ++                    adj.s ! AAdj Posit Nom ;
                  \\_ => noun.s ! Sg ! Nom ++ BIND++"-"++BIND ++ adj.s ! AAdj Posit Nom} ;
    isPre = True
    } ;

    FrontExtPredVP np vp = {
      s = \\t,a,b,o =>
        let
          subj  = np.s ! npNom ;
          agr   = np.a ;
          verb  = vp.s ! t ! a ! b ! o ! agr ;
          compl = vp.s2 ! agr
        in
        case o of {
          ODir _ => vp.ext ++ frontComma ++ subj ++ verb.aux ++ verb.adv ++ vp.ad ! agr ++ verb.fin ++ verb.inf ++ vp.p ++ compl ;
          OQuest => verb.aux ++ subj ++ verb.adv ++ vp.ad ! agr ++ verb.fin ++ verb.inf ++ vp.p ++ compl ++ vp.ext
          }
    } ;

    InvFrontExtPredVP np vp = {
      s = \\t,a,b,o =>
        let
          subj  = np.s ! npNom ;
          agr   = np.a ;
          verb  = vp.s ! t ! a ! b ! o ! agr ;
          compl = vp.s2 ! agr
        in
        case o of {
          ODir _ => vp.ext ++ verb.aux ++ verb.adv ++ vp.ad ! agr ++ verb.fin ++ subj ++ verb.inf ++ vp.p ++ compl ;
          OQuest => verb.aux ++ subj ++ verb.adv ++ vp.ad ! agr ++ verb.fin ++ verb.inf ++ vp.p ++ compl ++ vp.ext
          }
    } ;



  lin
    AdAdV = cc2 ;

    AdjAsCN ap = let cn = mkNoun "one" "one's" "ones" "ones'" ** {g = Neutr}
      in {
        s = \\n,c => preOrPost ap.isPre (ap.s ! agrgP3 n cn.g) (cn.s ! n ! c) ;
        g = cn.g
        } ;
    AdjAsNP ap = {
      s = \\c => ap.s ! agrgP3 Sg nonhuman ; ---- genitive case?
      a = agrgP3 Sg nonhuman
      } ;

    PositAdVAdj a = {s = a.s ! AAdv} ;

  lincat
    RNP     = {s : Agr => Str} ;
    RNPList = {s1,s2 : Agr => Str} ;

  lin
    ReflRNP vps rnp = insertObjPre (\\a => vps.c2 ++ rnp.s ! a) vps ;
    ReflPron = {s = reflPron} ;
    ReflPoss num cn = {s = \\a => possPron ! a ++ num.s ! True ! Nom ++ cn.s ! num.n ! Nom} ;
    PredetRNP predet rnp = {s = \\a => predet.s ++ rnp.s ! a} ;

    AdvRNP np prep rnp = {s = \\a => np.s ! NPAcc ++ prep.s ++ rnp.s ! a} ;
    AdvRVP vp prep rnp = insertObj (\\a => prep.s ++ rnp.s ! a) vp ;
    AdvRAP ap prep rnp = {s = \\a => ap.s ! a ++ prep.s ++ rnp.s ! a ; isPre = False} ;

    ReflA2RNP a rnp = {
      s = \\ag => a.s ! AAdj Posit Nom ++ a.c2 ++ rnp.s ! ag ;
      isPre = False
      } ;

    PossPronRNP pron num cn rnp = DetCN (DetQuant (PossPron pron) num) (PossNP cn (lin NP {s = \\_ => rnp.s ! pron.a; a = pron.a})) ;

    ConjRNP conj rpns = conjunctDistrTable Agr conj rpns ;

    Base_rr_RNP x y = twoTable Agr x y ;
    Base_nr_RNP x y = twoTable Agr {s = \\a => x.s ! NPAcc} y ;
    Base_rn_RNP x y = twoTable Agr x {s = \\a => y.s ! NPAcc} ;
    Cons_rr_RNP x xs = consrTable Agr comma x xs ;
    Cons_nr_RNP x xs = consrTable Agr comma {s = \\a => x.s ! NPAcc} xs ;

  lin
    ApposNP np1 np2 = {s = \\c => np1.s ! c ++ comma ++ np2.s ! c; a = np1.a} ;

    AdvIsNP adv np = PredVP {s = \\_ => adv.s ; a = np.a} (UseComp (CompNP np)) ;

---- TODO: RNPList construction

  lin
    ComplGenVV v a p vp = insertObj (\\agr => a.s ++ p.s ++
                                         infVP v.typ vp False a.a p.p agr)
                               (predVV v) ;

    CompS s = {s = \\_ => "that" ++ s.s} ;
    CompQS qs = {s = \\_ => qs.s ! QIndir} ;
    CompVP ant p vp = {s = \\a => ant.s ++ p.s ++
                                infVP VVInf vp False ant.a p.p a} ;

-- quite specific for English anyway

    UncontractedNeg = {s = [] ; p = CNeg False} ;
    UttVPShort vp = {s = infVP VVAux vp False Simul CPos (agrP3 Sg)} ;

lin UseDAP dap = {
      s = dap.sp ! Neutr ! False ;
      a = agrP3 dap.n
    } ;

lin UseDAPMasc dap = {
      s = dap.sp ! Masc ! False ;
      a = agrgP3 dap.n Masc
    } ;

lin UseDAPFem dap = {
      s = dap.sp ! Fem ! False ;
      a = agrgP3 dap.n Fem
    } ;

lin CardCNCard card cn =
  {s,sp = \\d,c => card.s ! d ! Nom ++ cn.s ! card.n ! c ; n = Pl} ;

}
