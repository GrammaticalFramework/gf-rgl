concrete ConjunctionBel of Conjunction = CatBel ** open ResBel in {

lin
  ConjS conj xs = {s = xs.s1 ++ conj.s ++ xs.s2} ;
  ConjRS conj xs = {s = xs.s1 ++ conj.s ++ xs.s2} ;
  ConjAP conj xs = {s = \\c,gn => xs.s1 ! c ! gn ++ conj.s ++ xs.s2 ! c ! gn} ;
  ConjNP conj xs = {
    s = \\c => xs.s1 ! c ++ conj.s ++ xs.s2 ! c ;
    a = {g=Masc; n=conj.n; p=P3}
  } ;
  ConjAdv conj xs = {s = xs.s1 ++ conj.s ++ xs.s2} ;
  ConjAdV conj xs = {s = xs.s1 ++ conj.s ++ xs.s2} ;
  ConjIAdv conj xs = {s = xs.s1 ++ conj.s ++ xs.s2} ;
  ConjCN conj xs = {
    s = \\c,n => xs.s1 ! c ! n ++ conj.s ++ xs.s2 ! c ! n ;
    voc = xs.voc ;
    g = xs.g
  } ;
  ConjDet conj xs = {
    s = \\c,g => xs.s1 ! c ! g ++ conj.s ++ xs.s2 ! c ! g ;
    n = conj.n
  } ;

  BaseS x y = {s1 = x.s; s2 = y.s} ;
  ConsS x xs = {s1 = x.s ++ "," ++ xs.s1; s2 = xs.s2} ;
  BaseRS x y = {s1 = x.s; s2 = y.s} ;
  ConsRS x xs = {s1 = x.s ++ "," ++ xs.s1; s2 = xs.s2} ;
  BaseAdv x y = {s1 = x.s; s2 = y.s} ;
  ConsAdv x xs = {s1 = x.s ++ "," ++ xs.s1; s2 = xs.s2} ;
  BaseAdV x y = {s1 = x.s; s2 = y.s} ;
  ConsAdV x xs = {s1 = x.s ++ "," ++ xs.s1; s2 = xs.s2} ;
  BaseIAdv x y = {s1 = x.s; s2 = y.s} ;
  ConsIAdv x xs = {s1 = x.s ++ "," ++ xs.s1; s2 = xs.s2} ;
  BaseNP x y = {
    s1 = x.s ;
    s2 = y.s
  } ;
  ConsNP x xs = {
    s1 = \\c => x.s ! c ++ "," ++ xs.s1 ! c ;
    s2 = xs.s2
  } ;
  BaseAP x y = {s1 = x.s; s2 = y.s} ;
  ConsAP x xs = {s1 = \\c,gn => x.s ! c ! gn ++ "," ++ xs.s1 ! c ! gn; s2 = xs.s2} ;
  BaseCN x y = {s1 = x.s; s2 = y.s; voc = x.voc ++ "," ++ y.voc; g = x.g} ;
  ConsCN x xs = {s1 = \\c,n => x.s ! c ! n ++ "," ++ xs.s1 ! c ! n; s2 = xs.s2; voc = x.voc ++ "," ++ xs.voc; g = xs.g} ;
  BaseDAP x y = {s1 = x.s; s2 = y.s} ;
  ConsDAP x xs = {s1 = \\c,g => x.s ! c ! g ++ "," ++ xs.s1 ! c ! g; s2 = xs.s2} ;

lincat
  [S] = {s1,s2 : Str} ;
  [RS] = {s1,s2 : Str} ;
  [Adv] = {s1,s2 : Str} ;
  [AdV] = {s1,s2 : Str} ;
  [IAdv] = {s1,s2 : Str} ;
  [NP] = {s1,s2 : Case => Str} ;
  [AP] = {s1,s2 : Case => GenNum => Str} ;
  [CN] = {s1,s2 : Case => Number => Str; voc : Str; g : Gender} ;
  [DAP] = {s1,s2 : Case => Gender => Str} ;

}
