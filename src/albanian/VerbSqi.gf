concrete VerbSqi of Verb = CatSqi ** open Prelude, ParamX, ResSqi, (I=IrregSqi) in {

oper
  verbVP : Verb -> VP = \v -> lin VP {
    indicative = \\t,n,p,_,_ => v.indicative ! t ! n ! p ;
    subjunctive = \\n,p,_,_ => subjunctiveForm v.vtype
      (v.indicative ! ResSqi.Pres ! Sg ! P1)
      (v.indicative ! ResSqi.Pres ! n ! p) n p ;
    imperative = \\n,_ => v.imperative ! n ;
    participle = \\_,c => v.participle ;
    pres_optative = \\n,p,c => v.pres_optative ! n ! p ;
    perf_optative = \\n,p,c => v.perf_optative ! n ! p ;
    pres_admirative = \\n,p,c => v.pres_admirative ! n ! p ;
    imperf_admirative = \\n,p,c => v.imperf_admirative ! n ! p ;
    vtype = v.vtype
  } ;

  addVP : VP -> (Agr => Case => Str) -> VP = \vp,x -> vp ** {
    indicative = \\t,n,p,g,c => vp.indicative ! t ! n ! p ! g ! c ++
      x ! {gn=genNum g n; p=p} ! c ;
    subjunctive = \\n,p,g,c => vp.subjunctive ! n ! p ! g ! c ++
      x ! {gn=genNum g n; p=p} ! c ;
    imperative = \\n,c => vp.imperative ! n ! c ++
      x ! {gn=genNum Masc n; p=P2} ! c ;
    participle = \\a,c => vp.participle ! a ! c ++ x ! a ! c ;
    pres_optative = \\n,p,c => vp.pres_optative ! n ! p ! c ++
      x ! {gn=genNum Masc n; p=p} ! c ;
    perf_optative = \\n,p,c => vp.perf_optative ! n ! p ! c ++
      x ! {gn=genNum Masc n; p=p} ! c ;
    pres_admirative = \\n,p,c => vp.pres_admirative ! n ! p ! c ++
      x ! {gn=genNum Masc n; p=p} ! c ;
    imperf_admirative = \\n,p,c => vp.imperf_admirative ! n ! p ! c ++
      x ! {gn=genNum Masc n; p=p} ! c
  } ;

  slashVP : Verb -> Compl -> VPSlash = \v,c -> lin VPSlash {
    indicative=\\t,n,p=>v.indicative!t!n!p;
    subjunctive=\\n,p=>subjunctiveForm v.vtype
      (v.indicative!ResSqi.Pres!Sg!P1)
      (v.indicative!ResSqi.Pres!n!p) n p;
    imperative=\\n=>v.imperative!n;
    participle=v.participle;
    pres_optative=v.pres_optative;
    perf_optative=v.perf_optative;
    pres_admirative=v.pres_admirative;
    imperf_admirative=v.imperf_admirative;
    c2=c;
    vtype=v.vtype
  } ;

  addSlash : VPSlash -> Str -> VPSlash = \sl,x -> sl ** {
    indicative = \\t,n,p => sl.indicative ! t ! n ! p ++ x ;
    subjunctive = \\n,p => sl.subjunctive ! n ! p ++ x ;
    imperative = \\n => sl.imperative ! n ++ x ;
    participle = sl.participle ++ x ;
    pres_optative = \\n,p => sl.pres_optative ! n ! p ++ x ;
    perf_optative = \\n,p => sl.perf_optative ! n ! p ++ x ;
    pres_admirative = \\n,p => sl.pres_admirative ! n ! p ++ x ;
    imperf_admirative = \\n,p => sl.imperf_admirative ! n ! p ++ x
  } ;

  slashToVP : VPSlash -> VP = \v -> lin VP {
    indicative = \\t,n,p,g,c => v.indicative ! t ! n ! p ;
    subjunctive = \\n,p,_,_ => v.subjunctive ! n ! p ;
    imperative = \\n,c => v.imperative ! n ;
    participle = \\_,c => v.participle ;
    pres_optative = \\n,p,c => v.pres_optative ! n ! p ;
    perf_optative = \\n,p,c => v.perf_optative ! n ! p ;
    pres_admirative = \\n,p,c => v.pres_admirative ! n ! p ;
    imperf_admirative = \\n,p,c => v.imperf_admirative ! n ! p ;
    vtype = v.vtype
  } ;

lin
  UseV v = verbVP v ;
  SlashV2a v = slashVP v v.c2 ;

  ComplSlash v np = addVP (slashToVP v) (\\_,_ => v.c2.s ++ np.s ! v.c2.c) ;

  ComplVS v s = addVP (verbVP v) (\\_,_ => "që" ++ s.s) ;
  ComplVQ v q = addVP (verbVP v) (\\_,_ => q.s) ;
  ComplVA v ap = addVP (verbVP v) (\\a,c =>
    ap.s ! Indef ! c ! agrGender a ! agrNumber a) ;
  ComplVV v vp = addVP (verbVP v) (\\a,_ =>
    "të" ++ vp.subjunctive ! agrNumber a ! a.p ! agrGender a ! Nom) ;

  Slash2V3 v np = addSlash (slashVP (lin Verb v) v.c3)
    (v.c2.s ++ np.s ! v.c2.c) ;
  Slash3V3 v np = addSlash (slashVP (lin Verb v) v.c2)
    (v.c3.s ++ np.s ! v.c3.c) ;
  SlashV2S v s = addSlash (slashVP (lin Verb v) v.c2) ("që" ++ s.s) ;
  SlashV2Q v q = addSlash (slashVP (lin Verb v) v.c2) q.s ;
  SlashV2A v ap = addSlash (slashVP (lin Verb v) v.c2)
    (ap.s ! Indef ! Nom ! Masc ! Sg) ;
  SlashV2V v vp = addSlash (slashVP (lin Verb v) v.c2)
    ("të" ++ vp.subjunctive ! Sg ! P3 ! Masc ! Nom) ;
  SlashVV v sl = sl ** {
    indicative = \\t,n,p => v.indicative ! t ! n ! p ++ "të" ++ sl.subjunctive ! Sg ! P3 ;
    subjunctive = \\n,p => subjunctiveForm v.vtype
      (v.indicative!ResSqi.Pres!Sg!P1)
      (v.indicative!ResSqi.Pres!n!p) n p ++ "të" ++ sl.subjunctive ! Sg ! P3
  } ;
  SlashV2VNP v np sl = sl ** {
    indicative = \\t,n,p => v.indicative ! t ! n ! p ++ v.c2.s ++ np.s ! v.c2.c ++ "të" ++ sl.subjunctive ! Sg ! P3 ;
    subjunctive = \\n,p => subjunctiveForm v.vtype
      (v.indicative!ResSqi.Pres!Sg!P1)
      (v.indicative!ResSqi.Pres!n!p) n p ++ v.c2.s ++ np.s ! v.c2.c ++ "të" ++ sl.subjunctive ! Sg ! P3
  } ;

  ReflVP sl = addVP (slashToVP sl) (\\_,_ => "veten") ;
  UseComp comp = addVP (verbVP (lin Verb I.jam_V)) (\\a,_ => comp.s ! a) ;
  UseCopula = verbVP (lin Verb I.jam_V) ;
  PassV2 v = addVP (verbVP (lin Verb I.jam_V)) (\\_,_ => v.participle) ;

  AdvVP vp adv = addVP vp (\\_,_ => adv.s) ;
  ExtAdvVP vp adv = addVP vp (\\_,_ => SOFT_BIND ++ "," ++ adv.s) ;
  AdVVP adv vp = vp ** {
    indicative = \\t,n,p,g,c => adv.s ++ vp.indicative ! t ! n ! p ! g ! c ;
    subjunctive = \\n,p,g,c => adv.s ++ vp.subjunctive ! n ! p ! g ! c ;
    imperative = \\n,c => adv.s ++ vp.imperative ! n ! c
  } ;
  AdvVPSlash sl adv = sl ** {
    indicative = \\t,n,p => sl.indicative ! t ! n ! p ++ adv.s ;
    subjunctive = \\n,p => sl.subjunctive ! n ! p ++ adv.s ;
    imperative = \\n => sl.imperative ! n ++ adv.s ;
    participle = sl.participle ++ adv.s
  } ;
  AdVVPSlash adv sl = sl ** {
    indicative = \\t,n,p => adv.s ++ sl.indicative ! t ! n ! p ;
    subjunctive = \\n,p => adv.s ++ sl.subjunctive ! n ! p ;
    imperative = \\n => adv.s ++ sl.imperative ! n
  } ;
  VPSlashPrep vp prep = lin VPSlash {
    indicative = \\t,n,p => vp.indicative ! t ! n ! p ! Masc ! Nom ;
    subjunctive = \\n,p => vp.subjunctive ! n ! p ! Masc ! Nom ;
    imperative = \\n => vp.imperative ! n ! Nom ;
    participle = vp.participle ! {gn=GSg Masc;p=P3} ! Nom ;
    pres_optative = \\n,p => vp.pres_optative ! n ! p ! Nom ;
    perf_optative = \\n,p => vp.perf_optative ! n ! p ! Nom ;
    pres_admirative = \\n,p => vp.pres_admirative ! n ! p ! Nom ;
    imperf_admirative = \\n,p => vp.imperf_admirative ! n ! p ! Nom ;
    vtype = vp.vtype ;
    c2 = prep
  } ;

  CompAP ap = {s = \\a => ap.s ! Indef ! Nom ! agrGender a ! agrNumber a} ;
  CompNP np = {s = \\_ => np.s ! Nom} ;
  CompAdv adv = {s = \\_ => adv.s} ;
  CompCN cn = {s = \\_ => cn.s ! Indef ! Nom ! Sg} ;
}
