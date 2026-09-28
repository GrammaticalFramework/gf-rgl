--# -path=.:../common:../abstract

concrete ExtendGla of Extend = CatGla
  ** ExtendFunctor - [
    VPS           -- finite VP's with tense and polarity
    , ListVPS
    , VPI
    , ListVPI -- infinitive VP's (TODO: with anteriority and polarity)
    , MkVPS
    , PredVPS

    -- excluded because RGL funs needed for them not implemented yet
    , SlashBareV2S
    , PredAPVP
    , ComplBareVS
    , AdvIsNP, AdvIsNPAP
    , CompBareCN
    , CompIQuant
    , ComplSlashPartLast
    , ComplDirectVQ
    , ComplDirectVS
    , DetNPFem, DetNPMasc
    , ExistCN, ExistMassCN, ExistPluralCN, ExistsNP
    , ExistIPQS, ExistNPQS, ExistS
    , PredIAdvVP
    , PrepCN
    , ReflPossPron
    , UttVP, UttVPShort, UttAccNP, UttDatNP, UttAccIP, UttDatIP
    , EmptyRelSlash, StrandQuestSlash, StrandRelSlash
    , SubjRelNP
    , UseComp_ser, UseComp_estar
    , iFem_Pron, weFem_Pron, youFem_Pron, youPlFem_Pron, youPolFem_Pron, youPolPlFem_Pron, youPolPl_Pron, theyFem_Pron, theyNeutr_Pron
    , GenModNP
    , PiedPipingQuestSlash, PiedPipingRelSlash, SubjunctRelCN
    , PresPartAP, PassVPSlash, PassAgentVPSlash, PastPartAP, PastPartAgentAP
    , ProgrVPSlash, CompoundN, GerundCN, GerundNP, GerundAdv, ApposNP
    , ReflPron, ReflPoss, AdvRNP, AdvRVP, AdvRAP, PossPronRNP
    , UseDAP, UseDAPFem, UseDAPMasc
    , MkVPS, BaseVPS, ConsVPS, ConjVPS, PredVPS
    , MkVPI, BaseVPI, ConsVPI, ConjVPI, ComplVPIVV
    , PositAdVAdj, ComplBareVS, ByVP
    , CompoundAP, BaseComp, ConsComp, ConjComp
    , BaseImp, ConsImp, ConjImp

  ] with (Grammar=GrammarGla)
  ** open ResGla, Prelude in {

lincat
  VPS = SS ;
  [VPS] = {s1,s2 : Str} ;
  VPI = SS ;
  [VPI] = {s1,s2 : Str} ;
  [Comp] = {s1,s2 : Str} ;
  [Imp] = {s1,s2 : Str} ;

lin
  MkVPS t p vp = {s = vp.s} ;
  PredVPS np vps = {s = vps.s ++ linNP np} ;
  BaseVPS x y = {s1 = x.s ; s2 = y.s} ;
  ConsVPS x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2} ;
  ConjVPS c xs = {s = c.s1 ++ xs.s1 ++ c.s2 ++ xs.s2} ;
  MkVPI vp = {s = vp.s} ;
  BaseVPI x y = {s1 = x.s ; s2 = y.s} ;
  ConsVPI x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2} ;
  ConjVPI c xs = {s = c.s1 ++ xs.s1 ++ c.s2 ++ xs.s2} ;
  ComplVPIVV vv vpi = appendVP vv ("a" ++ vpi.s) ;

  PresPartAP vp = invariantAP (AG ++ vp.noun) ;
  PassVPSlash vp = appendVP extendBiV ("air" ++ vp.participle) ;
  PassAgentVPSlash vp np = appendVP extendBiV ("air" ++ vp.participle ++ "le" ++ linNP np) ;
  PastPartAP vp = invariantAP vp.participle ;
  PastPartAgentAP vp np = invariantAP (vp.participle ++ "le" ++ linNP np) ;
  ProgrVPSlash vp = vp ;

  CompoundN n1 n2 = n2 ** {
    s = \\c,d,n => n1.s ! Gen ! Indef ! Sg ++ n2.s ! c ! d ! n ;
    voc = \\n => n1.s ! Gen ! Indef ! Sg ++ n2.voc ! n
    } ;
  GerundCN vp = stringN vp.noun ;
  GerundNP vp = emptyNP ** {s = \\_ => vp.noun ; voc = vp.noun} ;
  GerundAdv vp = {s = "le" ++ vp.noun} ;
  ApposNP x y = x ** {s = \\c => x.s ! c ++ "," ++ linNP y} ;

  ReflPron = lin RNP (lin NP (emptyNP ** {s = \\_ => "fhèin" ; voc = "fhèin"})) ;
  ReflPoss n cn = lin RNP (lin NP (emptyNP ** {
    s = \\c => cn.s ! c ! Def ! n.n ++ "fhèin" ;
    voc = cn.voc ! n.n ++ "fhèin" ; a = NotPron (DDef n.n Def)
    })) ;
  AdvRNP np prep rnp = rnp ** {s = \\c => np.s ! c ++ prepNP prep rnp} ;
  AdvRVP vp prep rnp = appendVP vp (prepNP prep rnp) ;
  AdvRAP ap prep rnp = addRAP ap (prepNP prep rnp) ;
  PossPronRNP pron num cn rnp = emptyNP ** {
    art = \\_ => pron.poss ;
    s = \\c => cn.s ! c ! Def ! num.n ++ rnp.s ! c ;
    voc = cn.voc ! num.n ++ rnp.voc ;
    a = NotPron (DPoss num.n pron.a)
    } ;

  UseDAP dap = dapNP dap Masc ;
  UseDAPMasc dap = dapNP dap Masc ;
  UseDAPFem dap = dapNP dap Fem ;
  ComplSlashPartLast vp np = appendVP vp (prepNP vp.c2 np) ;
  GenModNP num np cn = emptyNP ** {
    s = \\c => cn.s ! c ! Def ! num.n ++ np.art ! Gen ++ np.s ! Gen ;
    voc = cn.voc ! num.n ++ np.s ! Gen ; a = NotPron (DDef num.n Def)
    } ;
  EmptyRelSlash cls = {s = \\t,a,p => "a" ++ case <t,a,p,cls.pred.copular> of {
    <GPres,GSimul,GPos,True> => "tha" ++ cls.subj ++ cls.pred.complement ;
    <GPres,GSimul,GPos,False> => "tha" ++ cls.subj ++ AG ++ cls.pred.noun ;
    <GPast,GSimul,GPos,True> => "bha" ++ cls.subj ++ cls.pred.complement ;
    <GPast,GSimul,GPos,False> => "rinn" ++ cls.subj ++ cls.pred.noun ;
    <_,_,GPos,_> => "tha" ++ cls.subj ++ cls.pred.participle ;
    <_,_,GNeg,True> => "nach eil" ++ cls.subj ++ cls.pred.complement ;
    <_,_,GNeg,False> => "nach eil" ++ cls.subj ++ AG ++ cls.pred.noun
    }} ;
  UttVPShort vp = {s = vp.s} ;
  PositAdVAdj a = {s = "gu" ++ a.s ! ASg NOM Masc} ;
  ComplBareVS v s = appendVP v s.s ;
  ByVP vp = {s = "le" ++ vp.noun} ;
  CompoundAP n a = invariantAP (n.s ! Gen ! Indef ! Sg ++ a.s ! ASg NOM Masc) ;
  BaseComp x y = {s1 = x.s ; s2 = y.s} ;
  ConsComp x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2} ;
  ConjComp c xs = {s = c.s1 ++ xs.s1 ++ c.s2 ++ xs.s2} ;
  BaseImp x y = {s1 = x.s ; s2 = y.s} ;
  ConsImp x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2} ;
  ConjImp c xs = {s = c.s1 ++ xs.s1 ++ c.s2 ++ xs.s2} ;

oper
  invariantAP : Str -> LinAP = \s -> {s = \\_ => s ; voc = \\_ => s} ;
  addRAP : LinAP -> Str -> LinAP = \ap,x -> {s = \\f => ap.s ! f ++ x ; voc = \\g => ap.voc ! g ++ x} ;
  stringN : Str -> LinN = \s -> {s = \\_,_,_ => s ; voc = \\_ => s ; g = Masc} ;
  dapNP : LinDet -> Gender -> LinNP = \d,g -> emptyNP ** {
    s = \\c => d.s ! g ! c ; voc = d.sp ; a = NotPron d.dt
    } ;
  extendBiV : LinV = {
    s = "bi" ; conditional = table {Sg => "bhiodh" ; Pl => "bhiodh"} ;
    imperative = table {P1 => table {Sg => "bitheam" ; Pl => "bitheamaid"} ; P2 => table {Sg => "bi" ; Pl => "bithibh"} ; P3 => table {Sg => "bitheadh" ; Pl => "bitheadh"}} ;
    future = table {Indep => "bidh" ; Dep => "bi"} ; past = table {Indep => "bha" ; Dep => "robh"} ;
    noun = "bhith" ; participle = "air a bhith" ;
    copular = True ; complement = []
    } ;
}
