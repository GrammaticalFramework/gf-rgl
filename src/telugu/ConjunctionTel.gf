concrete ConjunctionTel of Conjunction =
  CatTel ** open ResTel, Coordination, Prelude in {
  lin
    BaseS x y = {s1 = x.s ; s2 = y.s} ;
    ConsS x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2} ;
    ConjS conj xs = {s = xs.s1 ++ conj.s2 ++ xs.s2} ;

    BaseAdv x y = {s1 = x.s ; s2 = y.s} ;
    ConsAdv x xs = {s1 = x.s ++ "," ++ xs.s1 ; s2 = xs.s2} ;
    ConjAdv conj xs = {s = xs.s1 ++ conj.s2 ++ xs.s2} ;

    BaseNP x y = {s1 = x.s ; s2 = y.s ; a = x.a} ;
    ConsNP x xs = {s1 = \\c => x.s ! c ++ "," ++ xs.s1 ! c ; s2 = xs.s2 ; a = xs.a} ;
    ConjNP conj xs = {
      s = \\c => xs.s1 ! c ++ conj.s2 ++ xs.s2 ! c ;
      a = case xs.a of {Ag g _ p => Ag g Pl p}
      } ;

    BaseAP x y = {s1 = x.s ; s2 = y.s} ;
    ConsAP x xs = {s1 = \\g,n,c => x.s ! g ! n ! c ++ "," ++ xs.s1 ! g ! n ! c ; s2 = xs.s2} ;
    ConjAP conj xs = {s = \\g,n,c => xs.s1 ! g ! n ! c ++ conj.s2 ++ xs.s2 ! g ! n ! c} ;

    BaseCN x y = {s1 = x.s ; s2 = y.s ; g = y.g} ;
    ConsCN x xs = {s1 = \\n,c => x.s ! n ! c ++ "," ++ xs.s1 ! n ! c ; s2 = xs.s2 ; g = xs.g} ;
    ConjCN conj xs = {s = \\n,c => xs.s1 ! n ! c ++ conj.s2 ++ xs.s2 ! n ! c ; g = xs.g} ;
--
--    ConjS = conjunctDistrSS ;
--
--    ConjAdv = conjunctDistrSS ;
--
--    ConjNP conj ss = conjunctDistrTable Case conj ss ** {
--      a = conjAgr (agrP3 conj.n) ss.a
--      } ;
--
--    ConjAP conj ss = conjunctDistrTable Agr conj ss ** {
--      isPre = ss.isPre
--      } ;
--
---- These fun's are generated from the list cat's.
--
--    BaseS = twoSS ;
--    ConsS = consrSS comma ;
--    BaseAdv = twoSS ;
--    ConsAdv = consrSS comma ;
--    BaseNP x y = twoTable Case x y ** {a = conjAgr x.a y.a} ;
--    ConsNP xs x = consrTable Case comma xs x ** {a = conjAgr xs.a x.a} ;
--    BaseAP x y = twoTable Agr x y ** {isPre = andB x.isPre y.isPre} ;
--    ConsAP xs x = consrTable Agr comma xs x ** {isPre = andB xs.isPre x.isPre} ;
--
  lincat
    [S] = {s1,s2 : Str} ;
    [Adv] = {s1,s2 : Str} ;
    [NP] = {s1,s2 : NPCase => Str ; a : Agr} ;
    [AP] = {s1,s2 : Gender => Number => Case => Str} ;
    [CN] = {s1,s2 : Number => Case => Str ; g : Gender} ;
--
}
