--# -path=alltenses:../common:../abstract:../romance
concrete ExtendSpa of Extend = CatSpa ** ExtendRomanceFunctor -
  [
 EmptyRelSlash,
 BaseVPI, ConsVPI, ConjVPI, MkVPI, ComplVPIVV,
 BaseComp, ConsComp, ConjComp, BaseImp, ConsImp, ConjImp,
 RNP, ReflRNP, ReflPron, ReflPoss, PredetRNP,
 AdvRNP, AdvRVP, AdvRAP, ReflA2RNP, PossPronRNP, ReflPossPron,
 ProgrVPSlash,
 GerundNP, GerundCN,
 CompVP,
 CompoundAP,
 CompoundN,
 ExistsNP,
 EmbedSSlash,
 GenRP,
 IAdvAdv,
 ICompAP,
 InOrderToVP,
 WithoutVP,
 iFem_Pron,
 theyFem_Pron,
 weFem_Pron,
 youFem_Pron,
 youPlFem_Pron,
 youPolFem_Pron,
 youPolPlFem_Pron,
 youPolPl_Pron,
 PassVPSlash, PassAgentVPSlash,
 UseComp_estar, UseComp_ser,
 ExistNP

    ]                   -- don't forget to put the names of your own
                       -- definitions here
  with
    (Grammar = GrammarSpa), (Syntax = SyntaxSpa), (ResRomance = ResSpa) **
  open
  GrammarSpa,
  ResSpa,
  MorphoSpa,
  Coordination,
  Prelude,
  ParadigmsSpa,
  (I=IrregSpa),
  (P=ParamX) in {
    -- put your own definitions here

  lin
    EmptyRelSlash = RelSlash IdRP ;

  lincat
    RNP = {s : Agr => Case => Str ; isPron : Bool} ;
    VPI = {s : Agr => Str} ;
    [VPI] = {s1,s2 : Agr => Str} ;

  lin
    MkVPI vp = {s = \\a => infVP vp RPos a} ;
    BaseVPI = twoTable Agr ;
    ConsVPI = consrTable Agr comma ;
    ConjVPI = conjunctDistrTable Agr ;
    ComplVPIVV vv vpi =
      insertComplement (\\a => prepCase vv.c2.c ++ vpi.s ! a) (predV vv) ;

  lincat [Comp] = {s1,s2 : Agr => Str ; cop : CopulaType} ;

  lin
    BaseComp x y = twoTable Agr x y ** {cop = x.cop} ;
    ConsComp xs x = consrTable Agr comma xs x ** xs ;
    ConjComp conj cs = conjunctDistrTable Agr conj cs ** {cop = cs.cop} ;

  lincat [Imp] = {s1,s2 : RPolarity => P.ImpForm => Gender => Str} ;

  lin
    BaseImp = twoTable3 RPolarity P.ImpForm Gender ;
    ConsImp = consrTable3 RPolarity P.ImpForm Gender comma ;
    ConjImp = conjunctDistrTable3 RPolarity P.ImpForm Gender ;

    -- A progressive construction around a slash would fill or displace its
    -- gap.  The simple form is idiomatic in the relative and interrogative
    -- contexts in which this constructor is used.
    ProgrVPSlash vps = vps ;

    -- Spanish uses the infinitive, not the gerund, as a nominal form.
    GerundNP vp = let agr = Ag Masc Sg P3 in heavyNP {
      s = \\_ => infVP vp RPos agr ;
      a = agr
      } ;

    GerundCN vp = {
      s = \\n => infVP vp RPos (Ag Masc n P3) ;
      g = Masc
      } ;

    ReflRNP v rnp = case <rnp.isPron,v.c2.isDir> of {
      <True,True> => insertRefl v ;
      _ => insertComplement
             (\\a => let agr = verbAgr a in
                    v.c2.s ++ rnp.s ! agr ! v.c2.c) v
      } ;

    ReflPron = {
      s = \\agr,c => reflPron agr.n agr.p c ;
      isPron = True
      } ;

    ReflPoss num cn = {
      s = \\agr,c => possCase cn.g num.n c ++
                       possDet agr num.n cn.g ++ cn.s ! num.n ;
      isPron = False
      } ;

    PredetRNP pred rnp = {
      s = \\agr,c => pred.s ! complAgr agr ! c ++ rnp.s ! agr ! pred.c ;
      isPron = False
      } ;

    AdvRNP np prep rnp = {
      s = \\agr,c => (np.s ! c).ton ++ prep.s ++ rnp.s ! agr ! prep.c ;
      isPron = False
      } ;

    AdvRVP vp prep rnp =
      insertComplement (\\a => prep.s ++ rnp.s ! verbAgr a ! prep.c) vp ;

    AdvRAP ap prep rnp = ap ** {
      s = \\af => ap.s ! af ++ prep.s ++
                    rnp.s ! (aform2aagr af ** {p = P3}) ! prep.c ;
      isPre = False
      } ;

    ReflA2RNP a rnp = a ** {
      s = \\af => a.s ! af ++ a.c2.s ++
                    rnp.s ! (aform2aagr af ** {p = P3}) ! a.c2.c ;
      isPre = False
      } ;

    PossPronRNP pron num cn rnp = heavyNP {
      s = \\c => possCase cn.g num.n c ++ pron.poss ! num.n ! cn.g ++
                  cn.s ! num.n ++ rnp.s ! pron.a ! ResSpa.genitive ;
      a = agrP3 cn.g num.n ;
      hasClit = False ;
      isNeg = False
      } ;

    ReflPossPron = PossPron he_Pron ;

    iFem_Pron     = agr2pron ! {g=Fem ; n=Sg ; p=P1} ;
    youFem_Pron   = agr2pron ! {g=Fem ; n=Sg ; p=P2} ;
    weFem_Pron    = agr2pron ! {g=Fem ; n=Pl ; p=P1} ;
    youPlFem_Pron = agr2pron ! {g=Fem ; n=Pl ; p=P2} ;
    theyFem_Pron  = agr2pron ! {g=Fem ; n=Pl ; p=P3} ;

    youPolFem_Pron = mkPronoun
      "usted" "la" "le" "usted"
      "su" "su" "sus" "sus"
      Fem Sg P3 ;
    youPolPl_Pron = mkPronoun
      "ustedes" "los" "les" "ustedes"
      "su" "su" "sus" "sus"
      Masc Pl P3 ;
    youPolPlFem_Pron = mkPronoun
      "ustedes" "las" "les" "ustedes"
      "su" "su" "sus" "sus"
      Fem Pl P3 ;

    ICompAP ap = {
      s =\\a => "qué tan" ++ ap.s ! (genNum2Aform a.g a.n) ;
      cop = serCopula
      } ;

    IAdvAdv adv = {
      s = "qué tan" ++ adv.s
      } ;

    EmbedSSlash s = {s = \\_ => "lo que" ++ s.s ! {g=Masc ; n=Sg} ! Indic} ;

    ExistsNP np =
      mkClause [] True False np.a
      (insertComplement (\\_ => (np.s ! Nom).ton)
         (predV (mkV "existir"))) ;

    CompoundN noun noun2 = { -- order is different because that's needed for correct translation from english
      s = \\n => noun2.s ! n ++
                 case noun2.relType of {
                   NRelPrep p => prepCase (CPrep p) ;  -- tasa de suicidio
                   NRelNoPrep => []                    -- connessione internet = internet connection
                 } ++
                 noun.s ! Sg ;
      g = noun2.g ;
      relType = noun2.relType
      } ;

    CompoundAP noun adj = {
      s = \\af => case (aform2aagr af) of {
        {n = n} => adj.s ! genNum2Aform noun.g n ++ "de" ++ noun.s ! n
        } ;
      isPre = adj.isPre ;
      copTyp = adj.copTyp
      } ;

    WithoutVP vp = {
      s = "sin" ++ infStr vp
      } ;

    InOrderToVP vp = {
      s = "para" ++ infStr vp
      } ;

    --TODO: actually use ant
    CompVP ant p vp = let
      neg = negation ! p.p
      in {
        s = \\agr => ant.s ++ p.s ++ "de" ++ neg.p1 ++ infVP vp RPos agr ;
        cop = serCopula
      } ;

lin UseComp_estar comp = insertComplement comp.s (predV I.estar_V) ;
    UseComp_ser comp = insertComplement comp.s (predV copula) ;

lin PassVPSlash vps = passVPSlash vps [] ;
    PassAgentVPSlash vps np = passVPSlash 
      vps (let by = <Grammar.by8agent_Prep : Prep> in by.s ++ (np.s ! by.c).ton) ;

oper
    possDet : Agr -> Number -> Gender -> Str = \agr,n,g ->
      case <agr.n,agr.p> of {
        <Sg,P1> => i_Pron.poss ! n ! g ;
        <Sg,P2> => youSg_Pron.poss ! n ! g ;
        <Sg,P3> => he_Pron.poss ! n ! g ;
        <Pl,P1> => we_Pron.poss ! n ! g ;
        <Pl,P2> => youPl_Pron.poss ! n ! g ;
        <Pl,P3> => they_Pron.poss ! n ! g
        } ;

    passVPSlash : VPSlash -> Str -> VP = \vps, agent -> 
      let auxvp = predV auxPassive 
      in
      vps ** {
         s = auxvp.s ;
         agr = auxvp.agr ;
         comp  = \\a => vps.comp ! a ++ (let agr = complAgr a in vps.s.s ! VPart agr.g agr.n) ++ agent ;
        } ;

} ;
