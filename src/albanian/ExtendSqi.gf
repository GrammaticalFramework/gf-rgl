concrete ExtendSqi of Extend = CatSqi **
  open Prelude, ParamX, ResSqi, GrammarSqi, (I=IrregSqi) in {

lincat
  RNP = {s : Agr => Case => Str; isPron : Bool} ;
  RNPList = {first,last : Agr => Case => Str} ;
  VPS = {s : Agr => Str} ;
  VPI = {s : Agr => Str} ;
  [VPS] = {first,last : Agr => Str} ;
  [VPI] = {first,last : Agr => Str} ;
  [Comp] = {first,last : Agr => Str} ;
  [Imp] = {first,last : Polarity => Number => Str} ;

oper
  baseVP : Verb -> VP = \v -> lin VP {
    indicative = \\t,n,p,_,_ => v.indicative ! t ! n ! p ;
    subjunctive = \\n,p,_,_ => subjunctiveForm v.vtype
      (v.indicative ! ResSqi.Pres ! Sg ! P1)
      (v.indicative ! ResSqi.Pres ! n ! p) n p ;
    imperative = \\n,_ => v.imperative ! n ;
    participle = \\_,_ => v.participle ;
    pres_optative = \\n,p,_ => v.pres_optative ! n ! p ;
    perf_optative = \\n,p,_ => v.perf_optative ! n ! p ;
    pres_admirative = \\n,p,_ => v.pres_admirative ! n ! p ;
    imperf_admirative = \\n,p,_ => v.imperf_admirative ! n ! p ;
    vtype = v.vtype
  } ;

  extAddVP : VP -> (Agr => Str) -> VP = \v,x -> v ** {
    indicative = \\t,n,p,g,c => v.indicative ! t ! n ! p ! g ! c ++
      x ! {gn=genNum g n; p=p} ;
    subjunctive = \\n,p,g,c => v.subjunctive ! n ! p ! g ! c ++
      x ! {gn=genNum g n; p=p} ;
    imperative = \\n,c => v.imperative ! n ! c ++
      x ! {gn=case n of {Sg=>GSg Masc; Pl=>GPl}; p=P2} ;
    participle = \\a,c => v.participle ! a ! c ++ x ! a
  } ;

  baseSlashVP : VPSlash -> VP = \v -> lin VP {
    indicative = \\t,n,p,_,_ => v.indicative ! t ! n ! p ;
    subjunctive = \\n,p,_,_ => v.subjunctive ! n ! p ;
    imperative = \\n,_ => v.imperative ! n ;
    participle = \\_,_ => v.participle ;
    pres_optative = \\n,p,_ => v.pres_optative ! n ! p ;
    perf_optative = \\n,p,_ => v.perf_optative ! n ! p ;
    pres_admirative = \\n,p,_ => v.pres_admirative ! n ! p ;
    imperf_admirative = \\n,p,_ => v.imperf_admirative ! n ! p ;
    vtype = v.vtype
  } ;

lin
  GenModNP n np cn = {
    s = \\c => cn.s ! Def ! c ! n.n ++
                 link_clitic ! Def ! c ! cn.g ! n.n ++ np.s ! Ablat ;
    a = agrgP3 cn.g n.n
  } ;

  EmptyRelSlash cl = {s=\\_,t,a,p=>"që"++cl.s!t!a!p++cl.c2.s} ;

  ReflPron = {
    s = \\a,c => case <agrNumber a,c> of {
      <Sg,Acc> => "veten" ; <Pl,Acc> => "veten" ; _ => "vetes"
    } ;
    isPron = True
  } ;
  ReflPoss n cn = {s=\\_,c=>cn.s!Def!c!n.n; isPron=False} ;
  PredetRNP p r = r ** {s=\\a,c=>p.s++r.s!a!c} ;
  AdvRNP np p r = {s=\\a,c=>np.s!c++p.s++r.s!a!p.c; isPron=False} ;
  AdvRVP vp p r = extAddVP vp (\\a=>p.s++r.s!a!p.c) ;
  AdvRAP ap p r = {
    s=\\sp,c,g,n=>ap.s!sp!c!g!n++p.s++r.s!(agrgP3 g n)!p.c
  } ;
  PossPronRNP p n cn r = {
    s=\\c=>cn.s!Def!c!n.n++link_clitic!Def!c!cn.g!n.n++r.s!p.a!Ablat;
    a=agrgP3 cn.g n.n
  } ;

  CompoundN a b = {
    s=\\sp,c,n=>(UseN b).s!sp!c!n++(UseN a).s!Indef!Ablat!Sg;
    g=b.g
  } ;
  CompoundAP n a = {
    s=\\sp,c,g,num=>(UseN n).s!Indef!Nom!Sg++(PositA a).s!sp!c!g!num
  } ;
  GerundCN vp = {s=\\_,_,_=>vp.participle!{gn=GSg Masc;p=P3}!Nom; g=Masc} ;
  GerundNP vp = {s=\\_=>vp.participle!{gn=GSg Masc;p=P3}!Nom; a={gn=GSg Masc;p=P3}} ;
  GerundAdv vp = {s="duke"++vp.participle!{gn=GSg Masc;p=P3}!Nom} ;
  WithoutVP vp = {s="pa"++vp.participle!{gn=GSg Masc;p=P3}!Nom} ;
  ByVP vp = {s="duke"++vp.participle!{gn=GSg Masc;p=P3}!Nom} ;
  ApposNP a b = a ** {s=\\c=>a.s!c++SOFT_BIND++","++b.s!c} ;
  PositAdVAdj a = {s=a.s!Nom!Masc!Sg} ;
  UttVPShort vp = {s=vp.imperative!Sg!Nom} ;
  ComplSlashPartLast sl np = ComplSlash sl np ;

  PresPartAP vp = {s=\\_,_,_,_=>"duke"++vp.participle!{gn=GSg Masc;p=P3}!Nom} ;
  PastPartAP sl = {s=\\_,_,_,_=>sl.participle} ;
  PastPartAgentAP sl np = {
    s=\\_,_,_,_=>sl.participle++"nga"++np.s!Nom
  } ;
  PassVPSlash sl = extAddVP (baseVP I.jam_V) (\\_=>sl.participle) ;
  PassAgentVPSlash sl np =
    extAddVP (baseVP I.jam_V) (\\_=>sl.participle++"nga"++np.s!Nom) ;
  ProgrVPSlash sl = sl ;
  ComplBareVS v s = extAddVP (baseVP v) (\\_=>s.s) ;

  MkVPS t p vp = {
    s=\\a=>(PredVP {s=\\_=>[];a=a} vp).s!t.t!t.a!p.p
  } ;
  BaseVPS x y = {first=x.s; last=y.s} ;
  ConsVPS x xs = {first=\\a=>x.s!a++","++xs.first!a; last=xs.last} ;
  ConjVPS c xs = {s=\\a=>xs.first!a++c.s++xs.last!a} ;
  PredVPS np v = {s=v.s!np.a} ;

  MkVPI vp = {s=\\a=>"të"++vp.subjunctive ! agrNumber a ! a.p ! agrGender a ! Nom} ;
  BaseVPI x y = {first=x.s; last=y.s} ;
  ConsVPI x xs = {first=\\a=>x.s!a++","++xs.first!a; last=xs.last} ;
  ConjVPI c xs = {s=\\a=>xs.first!a++c.s++xs.last!a} ;
  ComplVPIVV v x = extAddVP (baseVP v) (\\a=>x.s!a) ;

  BaseComp x y = {first=x.s; last=y.s} ;
  ConsComp x xs = {first=\\a=>x.s!a++","++xs.first!a; last=xs.last} ;
  ConjComp c xs = {s=\\a=>xs.first!a++c.s++xs.last!a} ;

  BaseImp x y = {first=x.s; last=y.s} ;
  ConsImp x xs = {
    first=\\p,n=>x.s!p!n++","++xs.first!p!n;
    last=xs.last
  } ;
  ConjImp c xs = {s=\\p,n=>xs.first!p!n++c.s++xs.last!p!n} ;

  UseDAP d = {s=\\_=>d.s; a=agrgP3 Masc d.n} ;
  UseDAPMasc = UseDAP ;
  UseDAPFem d = {s=\\_=>d.s; a=agrgP3 Fem d.n} ;
  TPastSimple = {s=[]; t=ParamX.Past} ;
}
