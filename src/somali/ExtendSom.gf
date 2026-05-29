--# -path=.:../common:../abstract

concrete ExtendSom of Extend = CatSom
  ** ExtendFunctor - [
       GenModNP, FocusObj, ComplDirectVS, ComplDirectVQ, ExistIPQS,
       BaseVPS, ConsVPS, MkVPS, ConjVPS, PredVPS, SQuestVPS,
       QuestVPS, BaseVPI, ConsVPI, MkVPI, ConjVPI, ComplVPIVV,
       PassVPSlash, PassAgentVPSlash, PastPartAP, PastPartAgentAP,
       ProgrVPSlash, PurposeVP, ReflRNP, ReflPron, ReflPoss,
       PredetRNP, ComplSlashPartLast, CompoundN, CompoundAP, GerundCN, GerundNP,
       GerundAdv, WithoutVP, ByVP, InOrderToVP, ApposNP, AdAdV,
       UttAdV, PositAdVAdj, CompS, CompQS
     ]
  with (Grammar=GrammarSom)
  ** open Prelude, ResSom, NounSom, GrammarSom, ParadigmsSom in {

lincat
  VPS = {s : Agreement => Bool => Str} ;
  [VPS] = {s1,s2 : Agreement => Bool => Str} ;
  VPI = {s : Str} ;
  [VPI] = {s1,s2 : Str} ;
  [Comp] = {s1,s2 : Agreement => Str ; stm : STM} ;
  [Imp] = {s1,s2 : Str} ;

lin

  MkVPS t p vp = {
    s = \\agr,b => (UseCl t p (PredVP (pronTable ! agr) vp)).s ! b
    } ;
  BaseVPS x y = {s1 = x.s ; s2 = y.s} ;
  ConsVPS x xs = xs ** {
    s1 = \\agr,b => x.s ! agr ! b ++ "," ++ xs.s1 ! agr ! b
    } ;
  ConjVPS co xs = {
    s = \\agr,b => co.s1 ++ xs.s1 ! agr ! b ++ co.s2 ! Indefinite ++ xs.s2 ! agr ! b
    } ;
  PredVPS np vps = lin S {s = \\b => vps.s ! np.a ! b} ;
  SQuestVPS np vps = {s = vps.s ! np.a ! False} ;
  QuestVPS ip vps = {s = ip.s ! Abs ++ vps.s ! ip.a ! False} ;

  MkVPI vp = {s = infVP vp} ;
  BaseVPI x y = {s1 = x.s ; s2 = y.s} ;
  ConsVPI x xs = xs ** {s1 = x.s ++ "," ++ xs.s1} ;
  ConjVPI co xs = {s = co.s1 ++ xs.s1 ++ co.s2 ! Indefinite ++ xs.s2} ;
  ComplVPIVV vv vpi = UseComp (CompAdv (mkAdv vpi.s)) ;

  BaseComp x y = {s1 = compStr x ; s2 = compStr y ; stm = x.stm} ;
  ConsComp x xs = xs ** {
    s1 = \\agr => compStr x ! agr ++ "," ++ xs.s1 ! agr
    } ;
  ConjComp co xs = {
    aComp = \\agr => co.s1 ++ xs.s1 ! agr ++ co.s2 ! Indefinite ++ xs.s2 ! agr ;
    nComp = [] ;
    compar = [] ;
    stm = xs.stm
    } ;

  BaseImp x y = {s1 = x.s ! Sg ! Pos ; s2 = y.s ! Sg ! Pos} ;
  ConsImp x xs = xs ** {
    s1 = x.s ! Sg ! Pos ++ "," ++ xs.s1
    } ;
  ConjImp co xs = {
    s = \\num,pol => co.s1 ++ xs.s1 ++ co.s2 ! Indefinite ++ xs.s2
    } ;

  -- : Num -> NP -> CN -> NP ; -- this man's car(s)
  GenModNP num np cn = DetCN (DetQuant IndefArt num) (genModCN cn np) ;

   -- : NP  -> SSlash  -> Utt ;   -- her I love -- Saeed p. 189-
  FocusObj np sslash = {s = sslash.s ! False ++ objpron np ! Abs} ;

  -- FocusAdv : Adv -> S       -> Utt ;   -- today I will sleep
  -- FocusAdV : AdV -> S       -> Utt ;   -- never will I sleep
	  -- FocusAP  : AP  -> NP      -> Utt ;   -- green was the tree

  UseDAP dap = DetNP dap ;
  UseDAPMasc dap = DetNP (dap ** {sp = \\_,c => dap.sp ! Masc ! c}) ;
  UseDAPFem dap = DetNP (dap ** {sp = \\_,c => dap.sp ! Fem ! c}) ;

  PassVPSlash vps = lin VP (passVP vps) ;
  PassAgentVPSlash vps np = AdvVP (lin VP (passVP vps)) (mkAdv (np.s ! Abs)) ;

  PresPartAP vp = partAP vp ;
  PastPartAP vps = partAP (lin VP (passVP vps)) ;
  PastPartAgentAP vps np = lin AP {
    s = \\af => np.s ! Abs ++ linVP (VRel (af2gn af)) Subord vps ;
    compar = []
    } ;

  CompoundN n1 n2 = n2 ** {
    s = \\nf => n1.s ! Indef Sg ++ n2.s ! nf
    } ;

  CompoundAP n a = lin AP {
    s = \\af => n.s ! Indef Sg ++ a.s ! af ;
    compar = []
    } ;

  GerundCN vp = strCN (infVP vp) ;
  GerundNP vp = MassNP (GerundCN vp) ;
  GerundAdv vp = mkAdv (infVP vp) ;
  ByVP vp = mkAdv ("adigoo" ++ infVP vp) ;
  WithoutVP vp = mkAdv (infVP vp ++ "la'aan") ;
  InOrderToVP vp = mkAdv ("si" ++ infVP vp) ;
  PurposeVP = InOrderToVP ;

  ApposNP np app = np ** {
    s = \\c => np.s ! c ++ "," ++ app.s ! Abs ;
    isPron = False
    } ;

  AdAdV ada adv = {s = ada.s ++ adv.s} ;
  UttAdV adv = {s = adv.s} ;
  PositAdVAdj a = {s = "si" ++ a.s ! AF Sg Abs} ;

  CompS s = CompAdv (mkAdv (s.s ! True)) ;
  CompQS qs = CompAdv (mkAdv qs.s) ;

  ProgrVPSlash = ProgrVP ;
  ComplSlashPartLast = ComplSlash ;

  ReflPron = lin NP (indeclNP "is") ;
  ReflPoss num cn = DetCN (DetQuant (PossPron he_Pron) num) cn ;
  ReflRNP vps rnp = ComplSlash vps rnp ;
  ReflA2RNP a2 rnp = ComplA2 a2 rnp ;
  PredetRNP pred rnp = PredetNP pred rnp ;
  AdvRNP np prep rnp = AdvNP np (PrepNP prep rnp) ;
  AdvRVP vp prep rnp = AdvVP vp (PrepNP prep rnp) ;
  AdvRAP ap prep rnp = AdvAP ap (PrepNP prep rnp) ;
  PossPronRNP pron num cn rnp =
    DetCN (DetQuant (PossPron pron) num) (PossNP cn rnp) ;

  RecipVPSlash = ReflVP ;
  RecipVPSlashCN vps cn = ReflVP vps ;

oper
  af2gn : AForm -> GenNum = \af -> case af of {
    AF Pl _ => PlInv ;
    AF Sg _ => SgMasc
    } ;

  partAP : VP -> AP = \vp -> lin AP {
    s = \\af => linVP (VRel (af2gn af)) Subord vp ;
    compar = []
    } ;

  strCN : Str -> CN = \s -> lin CN {
    s = table {
      Indef _ => s ;
      Def _ => s ;
      NomSg => s ;
      Numerative => s
      } ;
    gda = MM KA KA ;
    shortPoss = False ;
    mod = \\_,_,_ => [] ;
    modtype = NoMod ;
    isPoss = False ;
    a = Sg3 Masc ;
    isPron = False ;
    st = Indefinite ;
    empty = []
    } ;

  compStr : Complement -> Agreement => Str = \comp ->
    \\agr => comp.aComp ! agr ++ comp.nComp ++ comp.compar ;

} ;
