concrete ExtendKaz of Extend = CatKaz **
  open Prelude, ResKaz, ParadigmsKaz, GrammarKaz, (P = ParamX) in {

  lincat
    RNP = {s : Case => Str} ;
    RNPList = {s1,s2 : Case => Str} ;
    VPS, VPI = {s : Str} ;
    [VPS], [VPI], [Comp], [Imp] = {s1,s2 : Str} ;

  oper
    extendNP : Str -> NP = \s -> lin NP {s=\\_ => s; a={p=P3;n=Sg}} ;
    simpleRNP : Str -> RNP = \s -> lin RNP {s=\\_ => s} ;

  lin
    TPastSimple = {s=[]; t=P.Past} ;
    CompoundN a b = lin N {
      s=\\c,n => a.s ! Nom ! Sg ++ b.s ! c ! n;
      poss=\\o,p,n => a.s ! Nom ! Sg ++ b.poss ! o ! p ! n
      } ;
    GenModNP num np cn = {s=\\c => np.s ! Gen ++ possForm cn np.a.n (nounPerson np.a.p) num.n; a={p=P3;n=num.n}} ;
    ApposNP a b = {s=\\c => a.s ! c ++ "," ++ b.s ! c; a=a.a} ;
    ReflPron = simpleRNP "өзін" ;
    ReflPoss num cn = {s=\\c => cn.s ! c ! num.n} ;
    AdvRNP np prep r = {s=\\c => np.s ! c ++ r.s ! prep.c ++ prep.s} ;
    AdvRVP vp prep r = prefixVerb (r.s ! prep.c ++ prep.s) vp ;
    AdvRAP ap prep r = {s=r.s ! prep.c ++ prep.s ++ ap.s} ;
    PossPronRNP pron num cn r = {s=\\c => pron.s ! Gen ++ possForm cn pron.a.n (nounPerson pron.a.p) num.n ++ r.s ! c; a={p=P3;n=num.n}} ;
    ComplBareVS vs s = prefixVerb s.s vs ;
    PresPartAP vp={s=vp.infinitive}; PastPartAP vp={s=vp.infinitive};
    PastPartAgentAP vp np={s=np.s ! Instr ++ vp.infinitive};
    PassVPSlash vp=prefixVerb "" vp; PassAgentVPSlash vp np=prefixVerb (np.s ! Instr) vp;
    ProgrVPSlash vp=vp;
    GerundCN vp={s=\\_,_ => vp.infinitive; poss=\\_,_,_ => vp.infinitive};
    GerundNP vp=extendNP vp.infinitive; GerundAdv vp={s=vp.infinitive}; ByVP vp={s=vp.infinitive};
    ComplSlashPartLast vp np=prefixVerb (np.s ! vp.c2.c ++ vp.c2.s) vp;
    UttVPShort vp={s=vp.infinitive}; PositAdVAdj a={s=a.s}; CompoundAP n a={s=n.s!Nom!Sg++a.s};
    MkVPS temp pol vp={s=(mkClause (extendNP "") vp).past ! pol.p};
    BaseVPS a b={s1=a.s;s2=b.s}; ConsVPS a b={s1=a.s++","++b.s1;s2=b.s2};
    ConjVPS c xs={s=xs.s1++c.s++xs.s2}; PredVPS np vps={s=np.s!Nom++vps.s};
    MkVPI vp={s=vp.infinitive}; BaseVPI a b={s1=a.s;s2=b.s};
    ConsVPI a b={s1=a.s++","++b.s1;s2=b.s2}; ConjVPI c xs={s=xs.s1++c.s++xs.s2};
    ComplVPIVV vv vpi=prefixVerb vpi.s vv;
    UseDAP d=extendNP d.s; UseDAPMasc d=extendNP d.s; UseDAPFem d=extendNP d.s;
    EmptyRelSlash cl={s=cl.pres!P.Pos};
    BaseImp a b={s1=a.s!Pos!Informal!Sg;s2=b.s!Pos!Informal!Sg};
    ConjImp c xs={s=\\_,_,_ => xs.s1++c.s++xs.s2};
    BaseComp a b={s1=a.s;s2=b.s}; ConjComp c xs={s=xs.s1++c.s++xs.s2};
}
