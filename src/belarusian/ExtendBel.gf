--# -path=.:../abstract:../common:prelude
concrete ExtendBel of Extend = CatBel ** open ResBel, (R = ParamX), ParadigmsBel, Prelude in {

lincat
  VPS = {s : Agr => Str} ;
  [VPS] = {s1,s2 : Agr => Str} ;
  VPI = {s : Str} ;
  [VPI] = {s1,s2 : Str} ;
  VPS2 = {s : Agr => Str; c : Compl; post : Str} ;
  [VPS2] = {s1,s2 : Agr => Str; c : Compl; post : Str} ;
  VPI2 = {s : Str; c : Compl; post : Str} ;
  [VPI2] = {s1,s2 : Str; c : Compl; post : Str} ;
  [Comp] = {s1,s2 : Agr => Str} ;
  [Imp] = {s1,s2 : R.Polarity => Number => Str} ;
  RNP = {s : Case => Str} ;
  RNPList = {s1,s2 : Case => Str} ;
  X = {s : Str} ;

lin
  iFem_Pron      = mkPron "я" "мяне" "мне"  "мяне" "мне" "мной" Fem Sg P1 ;
  youFem_Pron    = mkPron "ты" "цябе" "табе" "цябе" "табе" "табой" Fem Sg P2 ;
  weFem_Pron     = mkPron "мы" "нас" "нам" "нас" "нас" "намі" Fem Pl P1 ;
  youPlFem_Pron  = mkPron "вы" "вас" "вам" "вас" "вас" "вамі" Fem Pl P2 ;
  theyFem_Pron   = mkPron "яны" "іх" "ім" "іх" "іх" "імі" Fem Pl P3 ;
  theyNeutr_Pron = mkPron "яны" "іх" "ім" "іх" "іх" "імі" Neuter Pl P3 ;
  youPolFem_Pron = mkPron "вы" "вас" "вам" "вас" "вас" "вамі" Fem Pl P2 ;
  youPolPl_Pron  = mkPron "вы" "вас" "вам" "вас" "вас" "вамі" Fem Pl P2 ;
  youPolPlFem_Pron = mkPron "вы" "вас" "вам" "вас" "вас" "вамі" Fem Pl P2 ;

  GenNP np = {s = \\_,_,_ => np.s ! Gen} ;
  GenIP ip = {s = \\_,_,_ => ip.s ! Gen} ;
  GenRP num cn = {s = cn.s ! Gen ! num.n ++ "якога"} ;
  GenModNP num np cn = {
    s = \\c => cn.s ! c ! num.n ++ np.s ! Gen ;
    a = {g=cn.g; n=num.n; p=P3}
  } ;
  GenModIP num ip cn = {
    s = \\c => cn.s ! c ! num.n ++ ip.s ! Gen ;
    a = {g=cn.g; n=num.n; p=P3}
  } ;

  CompBareCN cn = {s = \\a => cn.s ! Nom ! a.n} ;
  PiedPipingQuestSlash ip cl = {s = \\t,p => cl.c.s ++ ip.s ! cl.c.c ++ cl.s ! t ! p} ;
  PiedPipingRelSlash rp cl = {s = \\t,p => rp.s ++ cl.s ! t ! p} ;
  StrandQuestSlash ip cl = {s = \\t,p => cl.c.s ++ ip.s ! cl.c.c ++ cl.s ! t ! p} ;
  StrandRelSlash rp cl = {s = \\t,p => rp.s ++ cl.s ! t ! p} ;
  EmptyRelSlash cl = {s = \\t,p => "што" ++ cl.s ! t ! p} ;

  MkVPS temp pol vp = {s = \\a => vp.s ! anteriorTense temp.t temp.a ! pol.p ! a} ;
  ConjVPS conj xs = {s = \\a => xs.s1 ! a ++ conj.s ++ xs.s2 ! a} ;
  PredVPS np vps = {s = np.s ! Nom ++ vps.s ! np.a} ;
  SQuestVPS np vps = {s = np.s ! Nom ++ vps.s ! np.a} ;
  QuestVPS ip vps = {s = ip.s ! Nom ++ vps.s ! ip.a} ;
  RelVPS rp vps = {s = rp.s ++ vps.s ! defaultAgr} ;
  BaseVPS x y = {s1 = x.s; s2 = y.s} ;
  ConsVPS x xs = {s1 = \\a => x.s ! a ++ "," ++ xs.s1 ! a; s2 = xs.s2} ;

  ExistS temp pol np = {s = copula temp.t pol.p np.a ++ np.s ! Nom} ;
  ExistNPQS temp pol np = {s = copula temp.t pol.p np.a ++ np.s ! Nom} ;
  ExistIPQS temp pol ip = {s = copula temp.t pol.p ip.a ++ ip.s ! Nom} ;

  MkVPI vp = {s = vp.inf ! defaultAgr} ;
  ConjVPI conj xs = {s = xs.s1 ++ conj.s ++ xs.s2} ;
  ComplVPIVV vv vpi = {
    s = \\t,p,a => finiteVerb vv t p a ++ vpi.s ;
    inf = \\_ => vv.infinitive ++ vpi.s ;
    imp = \\p,n => neg p ++ vv.imperative ! n ++ vpi.s
  } ;
  BaseVPI x y = {s1 = x.s; s2 = y.s} ;
  ConsVPI x xs = {s1 = x.s ++ "," ++ xs.s1; s2 = xs.s2} ;

  MkVPS2 temp pol vp = {s = \\a => vp.s ! anteriorTense temp.t temp.a ! pol.p ! a; c = vp.c; post = vp.post} ;
  ConjVPS2 conj xs = {s = \\a => xs.s1 ! a ++ conj.s ++ xs.s2 ! a; c = xs.c; post = xs.post} ;
  ComplVPS2 vps np = {s = \\a => vps.s ! a ++ prepNP vps.c np ++ vps.post} ;
  ReflVPS2 vps rnp = {s = \\a => vps.s ! a ++ rnp.s ! vps.c.c ++ vps.post} ;
  BaseVPS2 x y = {s1 = x.s; s2 = y.s; c = x.c; post = x.post ++ y.post} ;
  ConsVPS2 x xs = {s1 = \\a => x.s ! a ++ "," ++ xs.s1 ! a; s2 = xs.s2; c = xs.c; post = x.post ++ xs.post} ;

  MkVPI2 vp = {s = vp.inf; c = vp.c; post = vp.post} ;
  ConjVPI2 conj xs = {s = xs.s1 ++ conj.s ++ xs.s2; c = xs.c; post = xs.post} ;
  ComplVPI2 vpi np = {s = vpi.s ++ prepNP vpi.c np ++ vpi.post} ;
  BaseVPI2 x y = {s1 = x.s; s2 = y.s; c = x.c; post = x.post ++ y.post} ;
  ConsVPI2 x xs = {s1 = x.s ++ "," ++ xs.s1; s2 = xs.s2; c = xs.c; post = x.post ++ xs.post} ;

  ConjComp conj xs = {s = \\a => xs.s1 ! a ++ conj.s ++ xs.s2 ! a} ;
  BaseComp x y = {s1 = x.s; s2 = y.s} ;
  ConsComp x xs = {s1 = \\a => x.s ! a ++ "," ++ xs.s1 ! a; s2 = xs.s2} ;
  ConjImp conj xs = {s = \\p,n => xs.s1 ! p ! n ++ conj.s ++ xs.s2 ! p ! n} ;
  BaseImp x y = {s1 = x.s; s2 = y.s} ;
  ConsImp x xs = {s1 = \\p,n => x.s ! p ! n ++ "," ++ xs.s1 ! p ! n; s2 = xs.s2} ;

  ProDrop pron = pron ;
  ICompAP ap = {s = ap.s ! Nom ! GSg Masc} ;
  IAdvAdv adv = {s = adv.s} ;
  CompIQuant iq = {s = iq.s ! Nom ! Masc ! Sg} ;
  PrepCN prep cn = {s = prep.s ++ cn.s ! prep.c ! Sg} ;

  FocusObj np ss = {s = np.s ! Acc ++ ss.s} ;
  FocusAdv adv s = {s = adv.s ++ s.s} ;
  FocusAdV adv s = {s = adv.s ++ s.s} ;
  FocusAP ap np = {s = ap.s ! Nom ! genNum np.a.g np.a.n ++ np.s ! Nom} ;

  PresPartAP vp = {
    s = \\_,gn => relativeNom gn ++ vp.s ! R.Pres ! R.Pos ! agrFromGenNum gn;
    adv = vp.inf ! defaultAgr;
    post = True
  } ;
  EmbedPresPart vp = {s = vp.inf ! defaultAgr} ;
  PastPartAP vp = vp.pass ;
  PastPartAgentAP vp np = {
    s = \\c,gn => vp.pass.s ! c ! gn ++ prepNP (mkPrep instrumental) np ;
    adv = vp.pass.adv ++ prepNP (mkPrep instrumental) np;
    post = False
  } ;
  PassVPSlash vp = {
    s = \\t,p,a => copula t p a ++ vp.pass.s ! Nom ! genNum a.g a.n ;
    inf = \\a => "быць" ++ vp.pass.s ! Nom ! genNum a.g a.n ;
    imp = \\p,_ => neg p ++ "будзь" ++ vp.pass.s ! Nom ! GSg Masc
  } ;
  PassAgentVPSlash vp np = addAdvVP (PassVPSlash vp) (prepNP (mkPrep instrumental) np) ;
  NominalizeVPSlashNP vp np = mkSimpleNP (vp.inf ++ prepNP vp.c np) Neuter Sg P3 ;
  ProgrVPSlash vp = vp ;
  A2VPSlash a = {
    s = \\t,p,agr => copula t p agr ++ a.s ! Nom ! genNum agr.g agr.n ;
    inf = a.s ! Nom ! GSg Masc ;
    c = a.c2 ;
    imp = \\p,_ => neg p ++ a.s ! Nom ! GSg Masc ;
    pass = a ;
    post = [] ;
    objPost = \\_ => []
  } ;
  N2VPSlash n = {
    s = \\t,p,agr => copula t p agr ++ n.s ! Nom ! agr.n ;
    inf = n.s ! Nom ! Sg ;
    c = n.c2 ;
    imp = \\p,_ => neg p ++ n.s ! Nom ! Sg ;
    pass = adjFromStr (n.s ! Nom ! Sg) ;
    post = [] ;
    objPost = \\_ => []
  } ;
  ExistsNP np = {s = \\t,p => copula t p np.a ++ np.s ! Nom} ;
  ExistCN cn = {s = \\t,p => copula t p {g=cn.g; n=Sg; p=P3} ++ cn.s ! Nom ! Sg} ;
  ExistMassCN cn = ExistCN cn ;
  ExistPluralCN cn = {s = \\t,p => copula t p {g=cn.g; n=Pl; p=P3} ++ cn.s ! Nom ! Pl} ;
  AdvIsNP adv np = {s = \\t,p => adv.s ++ copula t p np.a ++ np.s ! Nom} ;
  AdvIsNPAP adv np ap = {s = \\t,p => adv.s ++ copula t p np.a ++ np.s ! Nom ++ ap.s ! Nom ! genNum np.a.g np.a.n} ;
  PurposeVP vp = {s = "каб" ++ vp.inf ! defaultAgr} ;
  ComplBareVS vs s = {
    s = \\t,p,a => finiteVerb vs t p a ++ s.s ;
    inf = \\_ => vs.infinitive ++ s.s ;
    imp = \\p,n => neg p ++ vs.imperative ! n ++ s.s
  } ;
  SlashBareV2S v s = {
    s = \\t,p,a => finiteVerb v t p a ;
    inf = v.infinitive ++ s.s ;
    c = v.c2 ;
    imp = \\p,n => neg p ++ v.imperative ! n ;
    pass = v.pa ;
    post = s.s ;
    objPost = \\_ => s.s
  } ;
  ComplDirectVS vs utt = {
    s = \\t,p,a => finiteVerb vs t p a ++ utt.s ;
    inf = \\_ => vs.infinitive ++ utt.s ;
    imp = \\p,n => neg p ++ vs.imperative ! n ++ utt.s
  } ;
  ComplDirectVQ vq utt = {
    s = \\t,p,a => finiteVerb vq t p a ++ utt.s ;
    inf = \\_ => vq.infinitive ++ utt.s ;
    imp = \\p,n => neg p ++ vq.imperative ! n ++ utt.s
  } ;
  FrontComplDirectVS np vs utt = {s = \\t,p => utt.s ++ np.s ! Nom ++ finiteVerb vs t p np.a} ;
  FrontComplDirectVQ np vq utt = {s = \\t,p => utt.s ++ np.s ! Nom ++ finiteVerb vq t p np.a} ;
  PredAPVP ap vp = {s = \\t,p => copula t p defaultAgr ++ ap.s ! Nom ! GSg Neuter ++ vp.inf !  defaultAgr} ;
  AdjAsCN ap = nounFromStr (ap.s ! Nom ! GSg Masc) Masc ;
  AdjAsNP ap = mkSimpleNP (ap.s ! Nom ! GSg Masc) Masc Sg P3 ;
  PredIAdvVP iadv vp = {s = \\t,p => iadv.s ++ vp.inf ! defaultAgr} ;
  EmbedSSlash ss = {s = ss.s} ;

  ReflRNP vp rnp = {
    s = \\t,p,a => vp.s ! t ! p ! a ++ rnp.s ! vp.c.c ++ vp.post ;
    inf = \\_ => vp.inf ++ rnp.s ! vp.c.c ;
    imp = \\p,n => vp.imp ! p ! n ++ rnp.s ! vp.c.c ++ vp.post
  } ;
  ReflPron = {s = caseTable "сябе"} ;
  ReflPoss num cn = {s = \\c =>
    (mkPossAdj "св").s ! (modifierCase c cn.g num.n) ! genNum cn.g num.n ++ cn.s ! c ! num.n
  } ;
  PredetRNP pred rnp = {s = \\c => pred.s ! c ! Masc ! Sg ++ rnp.s ! c} ;
  AdvRNP np prep rnp = {s = \\c => np.s ! c ++ prep.s ++ rnp.s ! prep.c} ;
  AdvRVP vp prep rnp = addAdvVP vp (prep.s ++ rnp.s ! prep.c) ;
  AdvRAP ap prep rnp = {s = \\c,gn => ap.s ! c ! gn ++ prep.s ++ rnp.s ! prep.c; adv = ap.adv ++ prep.s ++ rnp.s ! prep.c; post = ap.post} ;
  ReflA2RNP a rnp = {s = \\c,gn => a.s ! c ! gn ++ a.c2.s ++ rnp.s ! a.c2.c; adv = a.adv ++ a.c2.s ++ rnp.s ! a.c2.c; post = a.post} ;
  PossPronRNP pron num cn rnp = {
    s = \\c => pron.poss ! c ! cn.g ! num.n ++ cn.s ! c ! num.n ++ rnp.s ! Gen ;
    a = {g=cn.g; n=num.n; p=P3}
  } ;
  ConjRNP conj xs = {s = \\c => xs.s1 ! c ++ conj.s ++ xs.s2 ! c} ;
  Base_rr_RNP x y = {s1 = x.s; s2 = y.s} ;
  Base_nr_RNP x y = {s1 = x.s; s2 = y.s} ;
  Base_rn_RNP x y = {s1 = x.s; s2 = y.s} ;
  Cons_rr_RNP x xs = {s1 = \\c => x.s ! c ++ "," ++ xs.s1 ! c; s2 = xs.s2} ;
  Cons_nr_RNP x xs = {s1 = \\c => x.s ! c ++ "," ++ xs.s1 ! c; s2 = xs.s2} ;
  ReflPossPron = mkQuant "свой" ;
  ComplGenVV vv ant pol vp = {
    s = \\t,p,a => finiteVerb vv t p a ++ neg pol.p ++ vp.inf ! a ;
    inf = \\a => vv.infinitive ++ neg pol.p ++ vp.inf ! a ;
    imp = \\p,n => neg p ++ vv.imperative ! n ++ neg pol.p ++ vp.inf ! defaultAgr
  } ;
  CompoundN n1 n2 = {
    s = \\c,n => n2.s ! c ! n ++ n1.s ! Gen ! Sg ;
    voc = n2.voc ++ n1.s ! Gen ! Sg ;
    g = n2.g
  } ;
  CompoundAP n a = {s = \\c,gn => n.s ! Nom ! Sg ++ a.s ! c ! gn; adv = n.s ! Nom ! Sg ++ a.adv; post = a.post} ;
  GerundCN vp = nounFromStr (vp.inf ! defaultAgr) Neuter ;
  GerundNP vp = mkSimpleNP (vp.inf ! defaultAgr) Neuter Sg P3 ;
  GerundAdv vp = {s = vp.inf ! defaultAgr} ;
  WithoutVP vp = {s = "без" ++ vp.inf ! defaultAgr} ;
  ByVP vp = {s = "праз" ++ vp.inf ! defaultAgr} ;
  InOrderToVP vp = {s = "каб" ++ vp.inf ! defaultAgr} ;
  ApposNP np app = {s = \\c => np.s ! c ++ app.s ! Nom; a = np.a} ;
  AdAdV ada adv = {s = ada.s ++ adv.s} ;
  UttAdV adv = {s = adv.s} ;
  PositAdVAdj a = {s = a.adv} ;
  CompS s = {s = \\_ => s.s} ;
  CompQS qs = {s = \\_ => qs.s} ;
  CompVP ant pol vp = {s = \\_ => neg pol.p ++ vp.inf ! defaultAgr} ;
  UncontractedNeg = {s = []; p = R.Neg} ;
  UttVPShort vp = {s = vp.inf ! defaultAgr} ;
  ComplSlashPartLast vp np = {
    s = \\t,p,a => vp.s ! t ! p ! a ++ prepNP vp.c np ++ vp.post ;
    inf = \\_ => vp.inf ++ prepNP vp.c np ;
    imp = \\p,n => vp.imp ! p ! n ++ prepNP vp.c np ++ vp.post
  } ;
  DetNPMasc det = {s = \\c => det.s ! c ! Masc; a = {g=Masc; n=det.n; p=P3}} ;
  DetNPFem det = {s = \\c => det.s ! c ! Fem; a = {g=Fem; n=det.n; p=P3}} ;
  UseComp_estar comp = UseComp_ser comp ;
  UseComp_ser comp = {
    s = \\t,p,a => copula t p a ++ comp.s ! a ;
    inf = \\a => "быць" ++ comp.s ! a ;
    imp = \\p,_ => neg p ++ "будзь" ++ comp.s ! defaultAgr
  } ;
  SubjRelNP np rs = {s = \\c => np.s ! c ++ rs.s; a = np.a} ;
  UttAccNP np = {s = np.s ! Acc} ;
  UttDatNP np = {s = np.s ! Dat} ;
  UttAccIP ip = {s = ip.s ! Acc} ;
  UttDatIP ip = {s = ip.s ! Dat} ;
  UseDAP dap = {s = \\c => dap.s ! c ! Masc; a = {g=Neuter; n=dap.n; p=P3}} ;
  UseDAPMasc dap = {s = \\c => dap.s ! c ! Masc; a = {g=Masc; n=dap.n; p=P3}} ;
  UseDAPFem dap = {s = \\c => dap.s ! c ! Fem; a = {g=Fem; n=dap.n; p=P3}} ;
  CardCNCard card cn = {s = card.s ++ cn.s ! Gen ! Pl; n = Pl} ;
  TPastSimple = {s = []; t = R.Past} ;
  SubjunctRelCN cn rs = {
    s = \\c,n => cn.s ! c ! n ++ rs.s ;
    voc = cn.voc ;
    g = cn.g
  } ;

}
