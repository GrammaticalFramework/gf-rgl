concrete ConjunctionSqi of Conjunction = CatSqi ** open ResSqi, Prelude, ParamX in {
lincat
  [S] = {first,last : Str} ;
  [RS] = {first,last : Agr => Str} ;
  [Adv] = {first,last : Str} ;
  [AdV] = {first,last : Str} ;
  [IAdv] = {first,last : Str} ;
  [NP] = {first,last : Case => Str; a : Agr} ;
  [AP] = {first,last : Species=>Case=>Gender=>Number=>Str} ;
  [CN] = {first,last : Species=>Case=>Number=>Str; g:Gender} ;
  [DAP] = {first,last : Str; n:Number} ;
lin
  BaseS x y={first=x.s;last=y.s}; ConsS x xs={first=x.s++","++xs.first;last=xs.last}; ConjS c xs={s=xs.first++c.s++xs.last};
  BaseAdv x y={first=x.s;last=y.s}; ConsAdv x xs={first=x.s++","++xs.first;last=xs.last}; ConjAdv c xs={s=xs.first++c.s++xs.last};
  BaseAdV x y={first=x.s;last=y.s}; ConsAdV x xs={first=x.s++","++xs.first;last=xs.last}; ConjAdV c xs={s=xs.first++c.s++xs.last};
  BaseIAdv x y={first=x.s;last=y.s}; ConsIAdv x xs={first=x.s++","++xs.first;last=xs.last}; ConjIAdv c xs={s=xs.first++c.s++xs.last};
  BaseNP x y={first=x.s;last=y.s;a={gn=GPl;p=P3}};
  ConsNP x xs={first=\\k=>x.s!k++","++xs.first!k;last=xs.last;a={gn=GPl;p=P3}};
  ConjNP c xs={s=\\k=>xs.first!k++c.s++xs.last!k;a=xs.a};
  BaseAP x y={first=x.s;last=y.s}; ConsAP x xs={first=\\sp,k,g,n=>x.s!sp!k!g!n++","++xs.first!sp!k!g!n;last=xs.last};
  ConjAP c xs={s=\\sp,k,g,n=>xs.first!sp!k!g!n++c.s++xs.last!sp!k!g!n};
  BaseRS x y={first=x.s;last=y.s}; ConsRS x xs={first=\\a=>x.s!a++","++xs.first!a;last=xs.last}; ConjRS c xs={s=\\a=>xs.first!a++c.s++xs.last!a};
  BaseCN x y={first=x.s;last=y.s;g=x.g}; ConsCN x xs={first=\\sp,k,n=>x.s!sp!k!n++","++xs.first!sp!k!n;last=xs.last;g=xs.g};
  ConjCN c xs={s=\\sp,k,n=>xs.first!sp!k!n++c.s++xs.last!sp!k!n;g=xs.g};
  BaseDAP x y={first=x.s;last=y.s;n=Pl}; ConsDAP x xs={first=x.s++","++xs.first;last=xs.last;n=Pl};
  ConjDet c xs={s=\\_,_=>xs.first++c.s++xs.last;post=\\_,_,_=>[];n=xs.n;sp=Def};
}
