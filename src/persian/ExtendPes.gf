--# -path=.:../common:../abstract:../prelude

concrete ExtendPes of Extend =
  CatPes ** ExtendFunctor - [
    VPS, ListVPS, BaseVPS, ConsVPS, ConjVPS, MkVPS, PredVPS, RelVPS,
    VPI, ListVPI, BaseVPI, ConsVPI, ConjVPI, MkVPI, ComplVPIVV,
    ListComp, BaseComp, ConsComp, ConjComp,
    ListImp, BaseImp, ConsImp, ConjImp,
    PresPartAP, PastPartAP, PastPartAgentAP,
    PassVPSlash, PassAgentVPSlash, ProgrVPSlash,
    ReflPron, ReflPoss, AdvRNP, AdvRVP, AdvRAP, PossPronRNP,
    CompoundN, CompoundAP, PositAdVAdj, UseDAP, UseDAPMasc, UseDAPFem,
    GenNP, ApposNP, ICompAP, AdvIsNP, InOrderToVP, ByVP, AdjAsNP, ComplBareVS
    ,GerundNP,GerundCN,GerundAdv,EmbedPresPart,EmbedSSlash
    ]
  with (Grammar=GrammarPes)
  ** open Prelude, ResPes, (M=MorphoPes) in {

lincat
  VPS = {s : Agr => Str} ;
  [VPS] = {s1,s2 : Agr => Str} ;
  VPI = {s : Str} ;
  [VPI] = {s1,s2 : Str} ;
  [Comp] = {s1,s2 : Agr => Str} ;
  [Imp] = {s1,s2 : Polarity => Number => Str} ;

lin
  MkVPS temp pol vp = {
    s = \\agr => (mkSClause [] agr vp).s ! Ind temp.t temp.a ! pol.p ! ODir
    } ;
  BaseVPS first second = {s1 = first.s ; s2 = second.s} ;
  ConsVPS first rest = {
    s1 = \\agr => first.s ! agr ++ SOFT_BIND ++ "،" ++ rest.s1 ! agr ;
    s2 = rest.s2
    } ;
  ConjVPS conj verbs = {
    s = \\agr => verbs.s1 ! agr ++ conj.s2 ++ verbs.s2 ! agr
    } ;
  PredVPS np verbs = {s = \\_ => np.s ! Bare ++ verbs.s ! np.a} ;
  RelVPS rp verbs = {
    s = \\agr => verbs.s ! agr ;
    rp = rp.s
    } ;

  MkVPI vp = {s = infVP vp} ;
  BaseVPI first second = {s1 = first.s ; s2 = second.s} ;
  ConsVPI first rest = {s1 = first.s ++ SOFT_BIND ++ "،" ++ rest.s1 ; s2 = rest.s2} ;
  ConjVPI conj verbs = {s = verbs.s1 ++ conj.s2 ++ verbs.s2} ;
  ComplVPIVV vv verbs = predV vv ** {
    vComp = \\_,_ => verbs.s ;
    vvtype = case vv.isDef of {True => DefVV ; False => FullVV}
    } ;

  BaseComp first second = {s1 = first.s ; s2 = second.s} ;
  ConsComp first rest = {
    s1 = \\agr => first.s ! agr ++ SOFT_BIND ++ "،" ++ rest.s1 ! agr ;
    s2 = rest.s2
    } ;
  ConjComp conj comps = {
    s = \\agr => comps.s1 ! agr ++ conj.s2 ++ comps.s2 ! agr
    } ;

  BaseImp first second = {s1 = first.s ; s2 = second.s} ;
  ConsImp first rest = {
    s1 = \\pol,num => first.s ! pol ! num ++ SOFT_BIND ++ "،" ++ rest.s1 ! pol ! num ;
    s2 = rest.s2
    } ;
  ConjImp conj imps = {
    s = \\pol,num => imps.s1 ! pol ! num ++ conj.s2 ++ imps.s2 ! pol ! num
    } ;

  PresPartAP vp = mkPartAP (infVP vp) ;
  PastPartAP slash = mkPartAP (passivePart slash) ;
  PastPartAgentAP slash agent =
    mkPartAP (passivePart slash ++ "توسط" ++ np2str agent) ;

  PassVPSlash slash = passVP slash ;
  PassAgentVPSlash slash agent = insertAdv ("توسط" ++ np2str agent) (passVP slash) ;
  ProgrVPSlash slash = predProg slash ** {c2 = slash.c2 ; agrObj = slash.agrObj} ;

  ReflPron = lin RNP (emptyNP ** {
    -- The invariant emphatic خود is valid with every subject and avoids
    -- incorrectly freezing this underspecified RNP to third-person خودش.
    s = \\_ => "خود" ;
    a = defaultAgr ;
    animacy = Animate ;
    lock_NP = <>
    }) ;
  ReflPoss num cn = lin RNP (emptyNP ** {
    s = \\m => cn.s ! num.n ! Ezafe ++ "خود" ++ cn.compl ! num.n ;
    a = agrP3 num.n ;
    animacy = cn.animacy ;
    lock_NP = <>
    }) ;
  AdvRNP np prep rnp = lin RNP (rnp ** {
    s = \\m => np.s ! Ezafe ++ appComp prep rnp.s ;
    a = np.a ;
    lock_NP = <>
    }) ;
  AdvRVP vp prep rnp = insertAdv (appComp prep rnp.s) vp ;
  AdvRAP ap prep rnp = ap ** {
    s = \\m => ap.s ! m ++ appComp prep rnp.s ;
    adv = ap.adv ++ appComp prep rnp.s
    } ;
  PossPronRNP pron num cn rnp = emptyNP ** {
    s = \\m => cn.s ! num.n ! Ezafe ++ rnp.s ! Bare ++ "توسط" ++ pron.s ;
    a = agrP3 num.n ;
    animacy = cn.animacy
    } ;

  CompoundN modifier head = head ** {
    s = \\n,m => head.s ! n ! Ezafe ++ modifier.s ! Sg ! Bare ;
    isCmpd = IsCmpd
    } ;
  CompoundAP noun adjective = {
    s = \\m => noun.s ! Sg ! Ezafe ++ adjective.s ! Positive ! m ;
    adv = noun.s ! Sg ! Bare ++ adjective.adv ;
    isPre = adjective.isPre ;
    afterPrefix = adjective.afterPrefix
    } ;
  PositAdVAdj adjective = {s = adjective.adv} ;

  UseDAP dap = indeclNP dap.s ** {a = agrP3 dap.n} ;
  UseDAPMasc dap = indeclNP dap.s ** {a = agrP3 dap.n ; animacy = Animate} ;
  UseDAPFem dap = indeclNP dap.s ** {a = agrP3 dap.n ; animacy = Animate} ;

  -- NP -> Quant ; -- this man's
  GenNP np = makeQuant [] [] Ezafe False ** np ** {
    mod = Ezafe ; -- the possessed will get Ezafe
    s = \\num,cmpd => np2str np -- possesser is unmarked; https://sites.la.utexas.edu/persian_online_resources/language-specific-grammar/ezfe/
  } ;

  -- : NP -> NP -> NP
  ApposNP np1 np2 = np1 ** {
    s = \\m => np1.s ! m ++ np2.s ! m
  } ;

  -- : AP -> NP
  AdjAsNP ap = emptyNP ** ap ;

  -- : VS  -> S  -> VP
  ComplBareVS vs s = embComp (s.s ! vs.compl) (predV vs) ;

  ICompAP ap = {s = "چقدر" ++ ap.s ! Bare} ;
  -- : VP -> CN ;          -- publishing of the document (can get a determiner)
  GerundCN vp = useN (indeclN (infVP vp)) ;

  -- : VP -> NP ;          -- publishing the document (by nature definite)
  GerundNP vp = indeclNP (infVP vp) ;

  -- : VP -> Adv ;         -- publishing the document (prepositionless adverb)
  GerundAdv vp = lin Adv {s = infVP vp} ;

  -- : VP -> SC ;
  EmbedPresPart vp = lin SC {s = infVP vp} ;

  -- : SSlash -> SC
  -- Not optimal: complement with آن should go after subject, but SSlash is already fixed.
  -- You can get the more idiomatic word order by using other RGL functions, so it's
  -- less critical to tweak this function and SSlash (pretty marginal category). /IL
  EmbedSSlash ss = {s = "آنچه" ++ appComp ss.c2 (\\_ => "آن") ++ ss.s ! Indic} ;

  -- : Adv -> NP -> Cl -- here is the car / here are the cars
  AdvIsNP adv np = mkClause (indeclNP adv.s ** {a = np.a}) (UseComp (CompNP np)) ;

  -- : VP -> Adv ;         -- by publishing the document
  ByVP vp = lin Adv {s = with_Prep.s ++ showVPH' VO False VVPres Inf defaultAgr vp } ;

  -- : VP -> Adv ;         -- (in order) to publish the document
  InOrderToVP vp = lin Adv {s = for_Prep.s
    ++ case vp.lightverb of {
         Kardan => showVPH PerfStem defaultAgr <vp ** {s = \\vf => []} : VP> ; -- only show prefix
         _ => showVPH PerfStem defaultAgr vp}
   } ;

oper
  mkPartAP : Str -> M.AP = \s -> {
    s = \\_ => s ;
    adv = s ;
    isPre = False ;
    afterPrefix = False
    } ;

  passivePart : VPH -> Str = \vp ->
    let passive = passVP vp
    in passive.prefix ++ passive.s ! PerfStem ;
}
