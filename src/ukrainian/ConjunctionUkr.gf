concrete ConjunctionUkr of Conjunction = CatUkr ** open ResUkr in {

lincat
  [S] = {s1,s2 : Str} ;
  [RS] = {s1,s2 : Gender => Number => Str} ;
  [Adv] = {s1,s2 : Str} ;
  [AdV] = {s1,s2 : Str} ;
  [IAdv] = {s1,s2 : Str} ;
  [NP] = {s1,s2 : Case => Str; g : Gender; n : Number; p : Person} ;
  [AP] = {s1,s2 : Case => GenNum => Str} ;
  [CN] = {s1,s2 : Case => Number => Str; g : Gender} ;
  [DAP] = {s1,s2 : Case => Gender => Str; n : Number} ;

lin
  BaseS x y = {s1=x.s; s2=y.s} ;
  ConsS x xs = {s1=x.s ++ "," ++ xs.s1; s2=xs.s2} ;
  ConjS conj xs = {s = conj.s1 ++ xs.s1 ++ conj.s2 ++ xs.s2} ;

  BaseRS x y = {s1=x.s; s2=y.s} ;
  ConsRS x xs = {s1=\\g,n => x.s ! g ! n ++ "," ++ xs.s1 ! g ! n; s2=xs.s2} ;
  ConjRS conj xs = {s = \\g,n => conj.s1 ++ xs.s1 ! g ! n ++ conj.s2 ++ xs.s2 ! g ! n} ;

  BaseAdv x y = {s1=x.s; s2=y.s} ;
  ConsAdv x xs = {s1=x.s ++ "," ++ xs.s1; s2=xs.s2} ;
  ConjAdv conj xs = {s = conj.s1 ++ xs.s1 ++ conj.s2 ++ xs.s2} ;

  BaseAdV x y = {s1=x.s; s2=y.s} ;
  ConsAdV x xs = {s1=x.s ++ "," ++ xs.s1; s2=xs.s2} ;
  ConjAdV conj xs = {s = conj.s1 ++ xs.s1 ++ conj.s2 ++ xs.s2} ;

  BaseIAdv x y = {s1=x.s; s2=y.s} ;
  ConsIAdv x xs = {s1=x.s ++ "," ++ xs.s1; s2=xs.s2} ;
  ConjIAdv conj xs = {s = conj.s1 ++ xs.s1 ++ conj.s2 ++ xs.s2} ;

  BaseAP x y = {s1=x.s; s2=y.s} ;
  ConsAP x xs = {s1=\\c,gn => x.s ! c ! gn ++ "," ++ xs.s1 ! c ! gn; s2=xs.s2} ;
  ConjAP conj xs = {
    s = \\c,gn => conj.s1 ++ xs.s1 ! c ! gn ++ conj.s2 ++ xs.s2 ! c ! gn
  } ;

  BaseNP x y = {s1=x.s; s2=y.s; g=y.g; n=Pl; p=y.p} ;
  ConsNP x xs = {
    s1=\\c => x.s ! c ++ "," ++ xs.s1 ! c ;
    s2=xs.s2 ;
    g=xs.g ;
    n=Pl ;
    p=xs.p
  } ;
  ConjNP conj xs = {
    s = \\c => conj.s1 ++ xs.s1 ! c ++ conj.s2 ++ xs.s2 ! c ;
    g = xs.g ;
    n = case conj.n of {Sg => xs.n ; Pl => Pl} ;
    p = xs.p
  } ;

  BaseCN x y = {s1=x.s; s2=y.s; g=y.g} ;
  ConsCN x xs = {
    s1=\\c,n => x.s ! c ! n ++ "," ++ xs.s1 ! c ! n ;
    s2=xs.s2 ;
    g=xs.g
  } ;
  ConjCN conj xs = {
    s = \\c,n => conj.s1 ++ xs.s1 ! c ! n ++ conj.s2 ++ xs.s2 ! c ! n ;
    g = xs.g ;
    voc = \\n => conj.s1 ++ xs.s1 ! Nom ! n ++ conj.s2 ++ xs.s2 ! Nom ! n
  } ;

  BaseDAP x y = {
    s1=x.s ;
    s2=y.s ;
    n=Pl
  } ;
  ConsDAP x xs = {
    s1=\\c,g => x.s ! c ! g ++ "," ++ xs.s1 ! c ! g ;
    s2=xs.s2 ;
    n=Pl
  } ;
  ConjDet conj xs = {
    s = \\c,g => conj.s1 ++ xs.s1 ! c ! g ++ conj.s2 ++ xs.s2 ! c ! g ;
    n = case conj.n of {Sg => xs.n ; Pl => Pl}
  } ;
}
