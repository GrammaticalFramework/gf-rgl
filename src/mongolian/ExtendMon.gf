--# -path=.:../common:../abstract

concrete ExtendMon of Extend =
  CatMon ** ExtendFunctor - [
    VPS, ListVPS, VPI, ListVPI,
    ListComp, ListImp,
    CompoundN, CompoundAP,
    PresPartAP, PastPartAP, PastPartAgentAP,
    PassVPSlash, PassAgentVPSlash, ProgrVPSlash,
    GerundCN, GerundNP, GerundAdv, WithoutVP, ByVP, InOrderToVP,
    ApposNP, PositAdVAdj,
    MkVPS, BaseVPS, ConsVPS, ConjVPS, PredVPS,
    MkVPI, BaseVPI, ConsVPI, ConjVPI, ComplVPIVV,
    BaseComp, ConsComp, ConjComp,
    BaseImp, ConsImp, ConjImp,
    ReflPron, ReflPoss, AdvRNP, AdvRVP, AdvRAP, ReflRNP, ReflA2RNP, PossPronRNP,
    UseDAP, UseDAPMasc, UseDAPFem
    ]
  with
    (Grammar = GrammarMon) **
  open Prelude, ResMon, ParadigmsMon in {

lincat
  VPS, VPI = {s : Str} ;
  [VPS], [VPI] = {s1, s2 : Str} ;
  [Comp] = {s1, s2 : RCase => Str} ;
  [Imp] = {s1, s2 : Polarity => ImpForm => Str} ;

lin
  CompoundN n1 n2 = {
    s = \\sf => n1.s ! SF Sg NNom ++ n2.s ! sf
    } ;

  CompoundAP n a = {s = n.s ! SF Sg NNom ++ a.s} ;

  PresPartAP vp = {
    s = vp.compl ! Nom ++ vp.embedCompl ! Nom ++ vp.adv ++
        (vp.s ! VPPart ClPres Pos Nom).fin
    } ;

  PastPartAP vp = {
    s = vp.compl ! Nom ++ vp.embedCompl ! Nom ++ vp.adv ++
        (vp.s ! VPPass Passive (ClPast Perf)).fin
    } ;

  PastPartAgentAP vp np = {
    s = np.s ! Inst ++ vp.compl ! Nom ++ vp.embedCompl ! Nom ++ vp.adv ++
        (vp.s ! VPPass Passive (ClPast Perf)).fin
    } ;

  PassVPSlash vp = {
    s = \\_ => vp.s ! VPPass Passive ClPres ;
    compl = vp.compl ; adv = vp.adv ; embedCompl = vp.embedCompl ; vt = vp.vt
    } ;

  PassAgentVPSlash vp np = insertObj (\\_ => np.s ! Inst) (PassVPSlash vp) ;
  ProgrVPSlash vp = vp ;

  GerundCN vp = {s = \\_,_ => infVP vp Nom} ;
  GerundNP vp = {
    s = \\rc => infVP vp rc ; n = Sg ; p = P3 ; isPron = False ; isDef = True
    } ;
  GerundAdv vp = {s = infVP vp Inst} ;
  WithoutVP vp = {s = infVP vp Nom ++ BIND ++ "гүйгээр"} ;
  ByVP vp = {s = infVP vp Inst} ;
  InOrderToVP vp = {s = infVP vp Dat ++ "тулд"} ;

  ApposNP np app = np ** {s = \\rc => np.s ! rc ++ "," ++ app.s ! Nom} ;
  PositAdVAdj a = a ;

  MkVPS temp pol vp = {
    s = vp.compl ! Nom ++ vp.embedCompl ! Nom ++ vp.adv ++
        (vp.s ! VPFin temp.t temp.a pol.p).fin ++
        (vp.s ! VPFin temp.t temp.a pol.p).aux
    } ;
  BaseVPS x y = {s1 = x.s ; s2 = y.s} ;
  ConsVPS x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2} ;
  ConjVPS conj xs = {s = xs.s1 ++ conj.s2 ++ xs.s2} ;
  PredVPS np vps = {s = \\_ => np.s ! Nom ++ vps.s} ;

  MkVPI vp = {s = infVP vp Nom} ;
  BaseVPI x y = {s1 = x.s ; s2 = y.s} ;
  ConsVPI x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2} ;
  ConjVPI conj xs = {s = xs.s1 ++ conj.s2 ++ xs.s2} ;
  ComplVPIVV vv vpi = insertObj (\\_ => vpi.s) (predV (auxToVerb vv)) ;

  BaseComp x y = {s1 = x.s ; s2 = y.s} ;
  ConsComp x xs = {
    s1 = \\rc => x.s ! rc ++ "," ++ xs.s1 ! rc ; s2 = xs.s2
    } ;
  ConjComp conj xs = {s = \\rc => xs.s1 ! rc ++ conj.s2 ++ xs.s2 ! rc} ;

  BaseImp x y = {s1 = x.s ; s2 = y.s} ;
  ConsImp x xs = {
    s1 = \\p,f => x.s ! p ! f ++ "," ++ xs.s1 ! p ! f ; s2 = xs.s2
    } ;
  ConjImp conj xs = {s = \\p,f => xs.s1 ! p ! f ++ conj.s2 ++ xs.s2 ! p ! f} ;

  ReflPron = lin RNP {
    s = reflPron ! Sg ; n = Sg ; p = P3 ; isPron = True ; isDef = True ; lock_NP = <>
    } ;
  ReflPoss num cn = lin RNP {
    s = \\rc => reflPron ! Sg ! Gen ++ cn.s ! num.n ! toNCase rc Definite ;
    n = num.n ; p = P3 ; isPron = False ; isDef = True ; lock_NP = <>
    } ;
  AdvRNP np prep rnp = lin RNP {
    s = \\rc => np.s ! rc ++ rnp.s ! prep.rc ++ prep.s ;
    n = np.n ; p = np.p ; isPron = False ; isDef = np.isDef ; lock_NP = <>
    } ;
  AdvRVP vp prep rnp = insertAdv (rnp.s ! prep.rc ++ prep.s) vp ;
  AdvRAP ap prep rnp = {s = rnp.s ! prep.rc ++ prep.s ++ ap.s} ;
  ReflRNP slash rnp = insertObj (\\_ => rnp.s ! slash.c2.rc ++ slash.c2.s) slash ;
  ReflA2RNP ap rnp = {s = rnp.s ! ap.c2.rc ++ ap.c2.s ++ ap.s} ;
  PossPronRNP pron num cn rnp = lin NP {
    s = \\rc => pron.s ! PronCase Gen ++ cn.s ! num.n ! NNom ++ rnp.s ! rc ;
    n = num.n ; p = P3 ; isPron = False ; isDef = True
    } ;

  UseDAP dap = {
    s = dap.s ; n = dap.n ; p = P3 ; isPron = False ; isDef = dap.isDef
    } ;
  UseDAPMasc = UseDAP ;
  UseDAPFem = UseDAP ;

}
