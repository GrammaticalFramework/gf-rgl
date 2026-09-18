concrete ConjunctionFao of Conjunction = CatFao ** open ResFao, Prelude in {

lincat
  [S] = {s1,s2 : Str} ;
  [RS] = {s1,s2 : Gender => PersNum => Str} ;
  [Adv], [AdV], [IAdv] = {s1,s2 : Str} ;
  [NP] = {s1,s2 : Case => Str ; g : Gender ; n : Number ; p : Person} ;
  [AP] = {s1,s2 : Declension => Gender => Number => Case => Str} ;
  [CN] = {s1,s2 : Species => Number => Case => Str ; p1,p2 : Number => Case => Str ; g : Gender} ;
  [DAP] = {s1,s2 : Gender => Case => Str ; n : Number ; sp : Species ; d : Declension} ;

lin
  ConjS conj xs = {s = xs.s1 ++ conj.s ++ xs.s2} ;
  ConjRS conj xs = {s = \\g,p => xs.s1 ! g ! p ++ conj.s ++ xs.s2 ! g ! p} ;
  ConjAdv conj xs = {s = xs.s1 ++ conj.s ++ xs.s2} ;
  ConjAdV conj xs = {s = xs.s1 ++ conj.s ++ xs.s2} ;
  ConjIAdv conj xs = {s = xs.s1 ++ conj.s ++ xs.s2} ;
  ConjNP conj xs = {
    s = \\c => xs.s1 ! c ++ conj.s ++ xs.s2 ! c ;
    g = xs.g ;
    n = Pl ;
    p = P3
  } ;
  ConjAP conj xs = {
    s = \\af,g,n,c => xs.s1 ! af ! g ! n ! c ++ conj.s ++ xs.s2 ! af ! g ! n ! c
  } ;
  ConjCN conj xs = {
    s = \\sp,n,c => xs.s1 ! sp ! n ! c ++ conj.s ++ xs.s2 ! sp ! n ! c ;
    p = \\n,c => xs.p1 ! n ! c ++ conj.s ++ xs.p2 ! n ! c ;
    g = xs.g
  } ;
  ConjDet conj xs = {
    s = \\g,c => xs.s1 ! g ! c ++ conj.s ++ xs.s2 ! g ! c ;
    n = xs.n ;
    sp = xs.sp ;
    d = xs.d
  } ;

  BaseS x y = {s1 = x.s ; s2 = y.s} ;
  ConsS x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2} ;
  BaseRS x y = {s1 = x.s ; s2 = y.s} ;
  ConsRS x xs = {s1 = \\g,p => x.s ! g ! p ++ "," ++ xs.s1 ! g ! p ; s2 = xs.s2} ;
  BaseAdv x y = {s1 = x.s ; s2 = y.s} ;
  ConsAdv x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2} ;
  BaseAdV x y = {s1 = x.s ; s2 = y.s} ;
  ConsAdV x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2} ;
  BaseIAdv x y = {s1 = x.s ; s2 = y.s} ;
  ConsIAdv x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2} ;
  BaseNP x y = {s1 = x.s ; s2 = y.s ; g = x.g ; n = Pl ; p = P3} ;
  ConsNP x xs = {s1 = \\c => x.s ! c ++ "," ++ xs.s1 ! c ; s2 = xs.s2 ; g = xs.g ; n = Pl ; p = P3} ;
  BaseAP x y = {s1 = x.s ; s2 = y.s} ;
  ConsAP x xs = {
    s1 = \\af,g,n,c => x.s ! af ! g ! n ! c ++ "," ++ xs.s1 ! af ! g ! n ! c ;
    s2 = xs.s2
  } ;
  BaseCN x y = {s1 = x.s ; s2 = y.s ; p1 = x.p ; p2 = y.p ; g = x.g} ;
  ConsCN x xs = {
    s1 = \\sp,n,c => x.s ! sp ! n ! c ++ "," ++ xs.s1 ! sp ! n ! c ; s2 = xs.s2 ;
    p1 = \\n,c => x.p ! n ! c ++ "," ++ xs.p1 ! n ! c ; p2 = xs.p2 ; g = xs.g
  } ;
  BaseDAP x y = {s1 = x.s ; s2 = y.s ; n = y.n ; sp = y.sp ; d = y.d} ;
  ConsDAP x xs = {s1 = \\g,c => x.s ! g ! c ++ "," ++ xs.s1 ! g ! c ; s2 = xs.s2 ; n = xs.n ; sp = xs.sp ; d = xs.d} ;
}
