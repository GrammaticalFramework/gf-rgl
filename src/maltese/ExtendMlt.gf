concrete ExtendMlt of Extend =
  CatMlt ** ExtendFunctor - [
                 ComplDirectVS,
                 ComplDirectVQ,
                 iFem_Pron, youFem_Pron, weFem_Pron, youPlFem_Pron,
                 theyFem_Pron, youPolFem_Pron,
                 MkVPS, ConjVPS, PredVPS, SQuestVPS, QuestVPS, RelVPS,
                 MkVPI, ConjVPI, ComplVPIVV,
                 MkVPS2, ConjVPS2, ComplVPS2, ReflVPS2,
                 MkVPI2, ConjVPI2, ComplVPI2,
                 BaseVPS, ConsVPS, BaseVPI, ConsVPI,
                 BaseVPS2, ConsVPS2, BaseVPI2, ConsVPI2,
                 BaseComp, ConsComp, ConjComp, BaseImp, ConsImp, ConjImp,
                 PresPartAP, EmbedPresPart, PastPartAP, PastPartAgentAP,
                 PassVPSlash, PassAgentVPSlash,
                 CompoundN, CompoundAP, GerundCN, GerundNP, GerundAdv,
                 WithoutVP, ByVP, InOrderToVP, ProgrVPSlash,
                 RNP, RNPList,
                 ReflRNP, ReflPron, ReflPoss, PredetRNP, AdvRNP, AdvRVP, AdvRAP,
                 ReflA2RNP, PossPronRNP, ConjRNP,
                 Base_rr_RNP, Base_nr_RNP, Base_rn_RNP, Cons_rr_RNP, Cons_nr_RNP,
                 AdAdV, UttAdV, PositAdVAd, UseDAP, UseDAPMasc, UseDAPFemj
              ]
              with (Grammar = GrammarMlt) ** open Prelude, Maybe, ParadigmsMlt, MorphoMlt, ResMlt in {

lin iFem_Pron = mkPron "jien"  "i" singular P1 feminine ; --- also JIENA
    youFem_Pron = mkPron "int" "ek" singular P2 feminine ; --- also INTI
    weFem_Pron = mkPron "aħna"  "na"  plural P1 feminine ;
    youPlFem_Pron = mkPron "intom" "kom" plural P2 feminine ;
    theyFem_Pron = mkPron "huma" "hom" plural P3 feminine ;
    youPolFem_Pron = mkPron "int" "ek" singular P2 feminine ; --- also INTI

lincat
  VPS = {s : Agr => Str} ;
  [VPS] = {s1,s2 : Agr => Str} ;
  VPI = {s : Agr => Str} ;
  [VPI] = {s1,s2 : Agr => Str} ;
  VPS2 = {s : Agr => Str ; c2 : Compl} ;
  [VPS2] = {s1,s2 : Agr => Str ; c2 : Compl} ;
  VPI2 = {s : Agr => Str ; c2 : Compl} ;
  [VPI2] = {s1,s2 : Agr => Str ; c2 : Compl} ;
  [Comp] = {s1,s2 : Agr => Str} ;
  [Imp] = {s1,s2 : Polarity => Number => Str} ;
  RNP = {s : Agr => Str} ;
  RNPList = {s1,s2 : Agr => Str} ;

lin BaseVPS x y = {s1 = x.s ; s2 = y.s} ;
    ConsVPS x xs = {
      s1 = \\agr => xs.s1 ! agr ++ "," ++ xs.s2 ! agr ;
      s2 = x.s
      } ;
    MkVPS temp pol vp = {s = \\agr => temp.s ++ pol.s ++ joinVP vp temp.t temp.a pol.p agr ++ vp.s2 ! agr} ;
    ConjVPS conj xs = {s = \\agr => conj.s1 ++ xs.s1 ! agr ++ conj.s2 ++ xs.s2 ! agr} ;
    PredVPS np vps = {s = np.s ! NPNom ++ vps.s ! np.a} ;

lin BaseVPI x y = {s1 = x.s ; s2 = y.s} ;
    ConsVPI x xs = {
      s1 = \\agr => xs.s1 ! agr ++ "," ++ xs.s2 ! agr ;
      s2 = x.s
      } ;
    MkVPI vp = {s = \\agr => infVP vp Simul Pos agr} ;
    ConjVPI conj xs = {s = \\agr => conj.s1 ++ xs.s1 ! agr ++ conj.s2 ++ xs.s2 ! agr} ;
    ComplVPIVV vv vpi = insertObj (\\agr => vpi.s ! agr) (predV vv) ;

lin BaseVPS2 x y = {s1 = x.s ; s2 = y.s ; c2 = y.c2} ;
    ConsVPS2 x xs = {
      s1 = \\agr => xs.s1 ! agr ++ "," ++ xs.s2 ! agr ;
      s2 = x.s ;
      c2 = x.c2
      } ;
    MkVPS2 temp pol vp = {
      s = \\agr => temp.s ++ pol.s ++ joinVP vp temp.t temp.a pol.p agr ++ vp.s2 ! agr ;
      c2 = vp.c2
      } ;
    ConjVPS2 conj xs = {
      s = \\agr => conj.s1 ++ xs.s1 ! agr ++ conj.s2 ++ xs.s2 ! agr ;
      c2 = xs.c2
      } ;
    ComplVPS2 vps np = {s = \\agr => vps.s ! agr ++ complNP vps.c2 np} ;

lin BaseVPI2 x y = {s1 = x.s ; s2 = y.s ; c2 = y.c2} ;
    ConsVPI2 x xs = {
      s1 = \\agr => xs.s1 ! agr ++ "," ++ xs.s2 ! agr ;
      s2 = x.s ;
      c2 = x.c2
      } ;
    MkVPI2 vp = {s = \\agr => infVP vp Simul Pos agr ; c2 = vp.c2} ;
    ConjVPI2 conj xs = {
      s = \\agr => conj.s1 ++ xs.s1 ! agr ++ conj.s2 ++ xs.s2 ! agr ;
      c2 = xs.c2
      } ;
    ComplVPI2 vpi np = {s = \\agr => vpi.s ! agr ++ complNP vpi.c2 np} ;

lin BaseComp x y = {s1 = x.s ; s2 = y.s} ;
    ConsComp x xs = {
      s1 = \\agr => xs.s1 ! agr ++ "," ++ xs.s2 ! agr ;
      s2 = x.s
      } ;
    ConjComp conj xs = {s = \\agr => conj.s1 ++ xs.s1 ! agr ++ conj.s2 ++ xs.s2 ! agr} ;

lin BaseImp x y = {s1 = x.s ; s2 = y.s} ;
    ConsImp x xs = {
      s1 = \\pol,num => xs.s1 ! pol ! num ++ "," ++ xs.s2 ! pol ! num ;
      s2 = x.s
      } ;
    ConjImp conj xs = {
      s = \\pol,num => conj.s1 ++ xs.s1 ! pol ! num ++ conj.s2 ++ xs.s2 ! pol ! num
      } ;

lin PresPartAP vp = {
      s = \\gn => "li" ++ infVP vp Simul Pos (toAgr gn) ;
      isPre = False
      } ;
    EmbedPresPart vp = {s = gerundStr vp} ;
    PastPartAP vp = {
      s = \\gn => pastPartStr vp gn ;
      isPre = False
      } ;
    PastPartAgentAP vp np = {
      s = \\gn => pastPartStr vp gn ++ prepNP prep_minn np ;
      isPre = False
      } ;
    PassVPSlash vp =
      insertObj (\\agr => pastPartStr vp (toGenNum agr)) CopulaVP ;
    PassAgentVPSlash vp np =
      insertObj (\\agr => pastPartStr vp (toGenNum agr) ++ prepNP prep_minn np) CopulaVP ;
    ProgrVPSlash vp = GrammarMlt.ProgrVP (lin VP vp) ** {c2 = vp.c2} ;

lin CompoundAP n a = {
      s = \\gn => n.s ! Singulative ++ a.s ! APosit gn ;
      isPre = False
      } ;
    CompoundN n1 n2 = {
      s = \\num => n2.s ! num ++ prep_ta.s ! Definite ++ n1.s ! Singulative ;
      g = n2.g ;
      hasColl = n2.hasColl ;
      hasDual = n2.hasDual ;
      takesPron = n2.takesPron
      } ;

lin SQuestVPS np vps = {s = \\q => case q of {
      QDir => vps.s ! np.a ++ np.s ! NPNom ;
      QIndir => np.s ! NPNom ++ vps.s ! np.a
      }} ;
    QuestVPS ip vps = {s = \\_ => ip.s ++ vps.s ! agrP3 ip.n Masc} ;
    RelVPS rp vps = {s = \\agr => rp.s ++ vps.s ! agr} ;

lin GerundCN vp = {
      s = \\_ => gerundStr vp ;
      g = Masc ;
      hasColl = False ;
      hasDual = False ;
      takesPron = False
      } ;
    GerundNP vp = {
      s = \\_ => gerundStr vp ;
      a = agrP3 Sg Masc ;
      isPron = False ;
      isDefn = True
      } ;
    GerundAdv vp = advSS (gerundStr vp) ;
    WithoutVP vp = advSS ("mingħajr" ++ gerundStr vp) ;
    ByVP vp = advSS ("billi" ++ infVP vp Simul Pos (agrP3 Sg Masc)) ;

lin ReflPron = {s = \\agr => prep_lil.enclitic ! agr ++ reflPron ! toVAgr agr} ;
    ReflPoss num cn = {s = \\agr => reflPossStr num cn agr} ;
    PredetRNP pred rnp = {s = \\agr => pred.s ++ rnp.s ! agr} ;
    AdvRNP np prep rnp = {s = \\agr => np.s ! NPAcc ++ prepRNP prep rnp agr} ;
    AdvRVP vp prep rnp = insertObj (\\agr => prepRNP prep rnp agr) vp ;
    AdvRAP ap prep rnp = {
      s = \\gn => ap.s ! gn ++ prepRNP prep rnp (toAgr gn) ;
      isPre = False
      } ;
    ReflA2RNP a2 rnp = {
      s = \\gn => a2.s ! APosit gn ++ complRNP a2.c2 rnp (toAgr gn) ;
      isPre = False
      } ;
    PossPronRNP p num cn rnp = {
      s = \\_ => num.s ! NumAdj ++ cn.s ! numform2nounnum num.n ++ p.s ! Possessive ++ prep_ta.s ! Definite ++ rnp.s ! p.a ;
      a = agrP3 (numform2num num.n) cn.g ;
      isPron = False ;
      isDefn = True
      } ;
    ConjRNP conj rnps = {s = \\agr => conj.s1 ++ rnps.s1 ! agr ++ conj.s2 ++ rnps.s2 ! agr} ;
    Base_rr_RNP x y = {s1 = x.s ; s2 = y.s} ;
    Base_nr_RNP np rnp = {s1 = \\_ => np.s ! NPAcc ; s2 = rnp.s} ;
    Base_rn_RNP rnp np = {s1 = rnp.s ; s2 = \\_ => np.s ! NPAcc} ;
    Cons_rr_RNP rnp rnps = {
      s1 = \\agr => rnps.s1 ! agr ++ "," ++ rnps.s2 ! agr ;
      s2 = rnp.s
      } ;
    Cons_nr_RNP np rnps = {
      s1 = \\agr => rnps.s1 ! agr ++ "," ++ rnps.s2 ! agr ;
      s2 = \\_ => np.s ! NPAcc
      } ;
    ReflPossPron = PossPron he_Pron ;

    PositAdVAdj a = {s = "b'mod" ++ a.s ! APosit (GSg Masc)} ;
    AdAdV ada adv = {s = ada.s ++ adv.s} ;
    UttAdV adv = {s = adv.s} ;

lin UseDAP dap = useDAP Masc dap ;
    UseDAPMasc dap = useDAP Masc dap ;
    UseDAPFem dap = useDAP Fem dap ;

oper
  complNP : Compl -> NP -> Str = \c,np ->
    case <c.isPresent,np.isDefn> of {
      <True,True>  => c.s ! Definite ++ np.s ! NPCPrep ;
      <True,False> => c.s ! Indefinite ++ np.s ! NPNom ;
      _            => np.s ! NPNom
      } ;

  complRNP : Compl -> {s : Agr => Str} -> Agr -> Str = \c,rnp,agr ->
    case c.isPresent of {
      True  => c.s ! Definite ++ rnp.s ! agr ;
      False => rnp.s ! agr
      } ;

  prepRNP : Prep -> {s : Agr => Str} -> Agr -> Str = \prep,rnp,agr ->
    prep.s ! Definite ++ rnp.s ! agr ;

  reflPossStr : Num -> CN -> Agr -> Str = \num,cn,agr ->
    num.s ! NumAdj ++ cn.s ! numform2nounnum num.n ++ prep_ta.enclitic ! agr ;

  pastPartStr : VPSlash -> GenNum -> Str = \vp,gn ->
    let agr : Agr = toAgr gn in
    case exists Participle vp.v.pastPart of {
      True  => fromJust Participle vp.v.pastPart ! gn ++ vp.s2 ! agr ++ vp.c2.s ! Definite ;
      False => (vp.v.s ! VPerf (toVAgr agr)).s1 ++ vp.s2 ! agr ++ vp.c2.s ! Definite
      } ;

  gerundStr : VP -> Str = \vp ->
    "li" ++ infVP vp Simul Pos (agrP3 Sg Masc) ;

  useDAP : Gender -> DAP -> NP = \g,dap -> lin NP {
    s = \\_ => dap.s ! g ++ dap.adj ! mkGenNum g (numform2num dap.n) ;
    a = agrP3 (numform2num dap.n) g ;
    isPron = False ;
    isDefn = dap.isDefn
    } ;

}
