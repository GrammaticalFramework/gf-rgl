concrete ConjunctionIna of Conjunction = 
  CatIna ** open ResIna, Coordination, Prelude in {

  flags optimize=all_subs ;

  lin

    ConjS = conjunctDistrSS ;

    ConjAdv = conjunctDistrSS ;

    ConjNP conj ss = conjunctDistrTable Case conj ss ** {
      isPronoun = False;
      a = {n = conjNumber conj.n ss.a.n ; p = ss.a.p}
      } ;

    ConjAP conj ss = conjunctDistrTable Agr conj ss ** {
      isPre = ss.isPre
      } ;

    ConjRS = conjunctDistrTable Agr ;

-- These fun's are generated from the list cat's.

    BaseS = twoSS ;
    ConsS = consrSS comma ;
    BaseAdv = twoSS ;
    ConsAdv = consrSS comma ;
    BaseNP x y = twoTable Case x y ** {a = conjAgr x.a y.a} ;
    ConsNP xs x = consrTable Case comma xs x ** {a = conjAgr xs.a x.a} ;
    BaseAP x y = twoTable Agr x y ** {isPre = andB x.isPre y.isPre} ;
    ConsAP xs x = consrTable Agr comma xs x ** {isPre = andB xs.isPre x.isPre} ;
    BaseRS x y = twoTable Agr x y ;
    ConsRS xs x = consrTable Agr comma xs x ;

  lincat
    [S] = {s1,s2 : Str} ;
    [Adv] = {s1,s2 : Str} ;
    [NP] = {s1,s2 : Case => Str ; a : Agr} ;
    [AP] = {s1,s2 : Agr => Str ; isPre : Bool} ;
    [RS] = {s1,s2 : Agr => Str} ;

}
