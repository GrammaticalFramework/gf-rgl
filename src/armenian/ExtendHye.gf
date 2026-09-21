--# -path=.:../abstract:../common:prelude

concrete ExtendHye of Extend =
  CatHye ** ExtendFunctor - [
    VPS, ListVPS, VPI, ListVPI,
    ListComp, ListImp,
    RNP, RNPList,
    GenModNP, EmptyRelSlash,
    MkVPS, BaseVPS, ConsVPS, ConjVPS, PredVPS, SQuestVPS, QuestVPS, RelVPS,
    ExistS, ExistNPQS, ExistIPQS,
    MkVPI, BaseVPI, ConsVPI, ConjVPI, ComplVPIVV,
    PresPartAP, EmbedPresPart, PastPartAP, PastPartAgentAP,
    PassVPSlash, PassAgentVPSlash, ProgrVPSlash, ComplBareVS,
    ReflPron, ReflPoss, PredetRNP, AdvRNP, AdvRVP, AdvRAP, PossPronRNP,
    Base_rr_RNP, Base_nr_RNP, Base_rn_RNP, Cons_rr_RNP, Cons_nr_RNP, ConjRNP,
    CompoundN, CompoundAP, GerundCN, GerundNP, GerundAdv, ByVP, ApposNP,
    PositAdVAdj, UttVPShort, ComplSlashPartLast,
    UseDAPMasc, UseDAPFem, UseDAP,
    DetNPMasc, DetNPFem, PredAPVP, SubjRelNP,
    BaseComp, ConsComp, ConjComp, BaseImp, ConsImp, ConjImp,
    TPastSimple
  ] with (Grammar = GrammarHye) **
  open Prelude, ResHye, ParadigmsHye, (R = ParamX) in {

lincat
  VPS = {s : Str} ; [VPS] = {s1,s2 : Str} ;
  VPI = {s : Str} ; [VPI] = {s1,s2 : Str} ;
  [Comp] = {s1,s2 : Str} ; [Imp] = {s1,s2 : Str} ;
  RNP = {s : Case => Str} ; RNPList = {s : Case => Str} ;

oper
  extToVP : Verb -> VP = \v -> lin VP {s=v.s;conditional=v.conditional;converb=v.converb;
    imperative=v.imperative;passive=v.passive;past=v.past;participle=v.participle;
    subjunctive=v.subjunctive} ;
  extAddVP : VP -> Str -> VP = \v,x -> lin VP {s=v.s ++ x;
    conditional=\\a,p,n=>v.conditional!a!p!n ++ x;
    converb={imperfective=v.converb.imperfective++x;futCon1=v.converb.futCon1++x;
      futCon2=v.converb.futCon2++x;negative=v.converb.negative++x;
      perfective=v.converb.perfective++x;simultaneous=v.converb.simultaneous++x};
    imperative=\\n=>v.imperative!n++x;passive=v.passive++x;
    past=\\p,n=>v.past!p!n++x;participle=\\p=>v.participle!p++x;
    subjunctive=\\a,p,n=>v.subjunctive!a!p!n++x} ;
  refl : RNP = lin RNP {s=\\_=>"իրեն"} ;

lin
  GenModNP num np cn = {s=\\c=>np.s!Dat ++ cn.s!Indef!c!num.n;a={n=num.n;p=P3}} ;
  EmptyRelSlash slash = {s="որը" ++ slash.s} ;

  MkVPS temp pol vp = {s=pol.s ++ vp.s} ;
  BaseVPS x y = {s1=x.s;s2=y.s} ;
  ConsVPS x xs = {s1=x.s;s2=xs.s1++","++xs.s2} ;
  ConjVPS c xs = {s=xs.s1++c.s++xs.s2} ;
  PredVPS np vps = {s=np.s!Nom++vps.s} ;
  SQuestVPS np vps = {s=np.s!Nom++vps.s++"՞"} ;
  QuestVPS ip vps = {s=ip.s++vps.s++"՞"} ;
  RelVPS rp vps = {s=rp.s++vps.s} ;
  ExistS t p np = {s=np.s!Nom++"կա"} ;
  ExistNPQS t p np = {s=np.s!Nom++"կա՞"} ;
  ExistIPQS t p ip = {s=ip.s++"կա՞"} ;

  MkVPI vp = {s=vp.s} ;
  BaseVPI x y = {s1=x.s;s2=y.s} ;
  ConsVPI x xs = {s1=x.s;s2=xs.s1++","++xs.s2} ;
  ConjVPI c xs = {s=xs.s1++c.s++xs.s2} ;
  ComplVPIVV vv vpi = extAddVP (extToVP vv) vpi.s ;

  PresPartAP vp = {s=\\_,_,_=>vp.participle!Subject;isPre=True} ;
  EmbedPresPart vp = {s=vp.participle!Subject} ;
  PastPartAP slash = {s=\\_,_,_=>slash.participle!Resultative;isPre=True} ;
  PastPartAgentAP slash np = {s=\\_,_,_=>slash.participle!Resultative++np.s!Instr;isPre=True} ;
  PassVPSlash slash = extAddVP (extToVP (mkV "լինել")) (slash.participle!Resultative) ;
  PassAgentVPSlash slash np = extAddVP (extToVP (mkV "լինել")) (slash.participle!Resultative++np.s!Instr) ;
  ProgrVPSlash slash = slash ;
  ComplBareVS vs s = extAddVP (extToVP vs) s.s ;

  ReflPron = refl ;
  ReflPoss num cn = {s=\\c=>"իր"++cn.s!Indef!c!num.n} ;
  PredetRNP pred r = {s=\\c=>pred.s++r.s!c} ;
  AdvRNP np prep r = {s=\\c=>np.s!c++prep.s++r.s!prep.c} ;
  AdvRVP vp prep r = extAddVP vp (prep.s++r.s!prep.c) ;
  AdvRAP ap prep r = {s=\\sp,c,n=>ap.s!sp!c!n++prep.s++r.s!prep.c;isPre=ap.isPre} ;
  PossPronRNP pron num cn r = {s=\\c=>pron.empty++cn.s!Indef!c!num.n++r.s!Dat;a={n=num.n;p=P3}} ;
  Base_rr_RNP x y = {s=\\c=>x.s!c++y.s!c} ;
  Base_nr_RNP x y = {s=\\c=>x.s!c++y.s!c} ;
  Base_rn_RNP x y = {s=\\c=>x.s!c++y.s!c} ;
  Cons_rr_RNP x xs = {s=\\c=>x.s!c++xs.s!c} ;
  Cons_nr_RNP x xs = {s=\\c=>x.s!c++xs.s!c} ;
  ConjRNP conj xs = {s=\\c=>conj.s++xs.s!c} ;

  CompoundN n1 n2 = lin N {s=\\c,n=>n1.s!Nom!Sg++n2.s!c!n;def_dat=\\n=>n1.s!Nom!Sg++n2.def_dat!n;
    def_nom=\\n=>n1.s!Nom!Sg++n2.def_nom!n;poss1=\\c,n=>n1.s!Nom!Sg++n2.poss1!c!n;poss2=\\c,n=>n1.s!Nom!Sg++n2.poss2!c!n} ;
  CompoundAP n a = {s=\\sp,c,num=>n.s!Nom!Sg++a.s!c!num;isPre=True} ;
  GerundCN vp = {s=\\_,_,_=>vp.s} ;
  GerundNP vp = {s=\\_=>vp.s;a={n=Sg;p=P3}} ;
  GerundAdv vp = {s=vp.s} ;
  ByVP vp = {s=vp.s} ;
  ApposNP np app = {s=\\c=>np.s!c++","++app.s!Nom;a=np.a} ;
  PositAdVAdj a = {s=a.s!Instr!Sg} ;
  UttVPShort vp = {s=vp.s} ;
  ComplSlashPartLast slash np = extAddVP (extToVP slash) (slash.c2.s++np.s!slash.c2.c) ;
  UseDAPMasc dap = {s=\\_=>dap.s;a={n=Sg;p=P3}} ;
  UseDAPFem dap = {s=\\_=>dap.s;a={n=Sg;p=P3}} ;
  UseDAP dap = {s=\\_=>dap.s;a={n=Sg;p=P3}} ;
  DetNPMasc = variants {} ;
  DetNPFem = variants {} ;
  PredAPVP = variants {} ;
  SubjRelNP = variants {} ;

  BaseComp x y = {s1=x.s;s2=y.s} ;
  ConsComp x xs = {s1=x.s;s2=xs.s1++","++xs.s2} ;
  ConjComp c xs = {s=xs.s1++c.s++xs.s2} ;
  BaseImp x y = {s1=x.s;s2=y.s} ;
  ConsImp x xs = {s1=x.s;s2=xs.s1++","++xs.s2} ;
  ConjImp c xs = {s=xs.s1++c.s++xs.s2} ;
  TPastSimple = {s=[];t=R.Past} ;
}
