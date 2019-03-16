
concrete ConjunctionPes of Conjunction =
  CatPes ** open ResPes, Coordination, Prelude in {


  flags optimize=all_subs ;

  lin

    ConjS  = conjunctDistrTable VVForm  ;

    ConjAdv = conjunctDistrSS ;
--    ConjAdv conj advs = conjunctDistrTable Gender conj advs ;

    ConjNP conj ss = ss ** conjunctDistrTable Mod conj ss ** {
      a = conjAgr (agrP3 conj.n) ss.a ;
      animacy = ss.animacy ;
      } ;

    ConjAP conj ss = ss ** conjunctDistrTable Mod conj ss ; -- Adv isn't changed
    ConjRS conj rs = conjunctDistrTable Agr conj rs ** { c = rs.c};

---- These fun's are generated from the list cat's.

    BaseS = twoTable VVForm ;
    ConsS = consrTable VVForm comma ;

    BaseAdv = twoSS ;
    ConsAdv = consrSS comma ;

    BaseNP x y = y ** twoTable Mod x y ** {a = conjAgr x.a y.a ; animacy = y.animacy } ; -- check animacy
    BaseRS x y = twoTable Agr x y ** {c = x.c};
    ConsNP xs x = xs ** consrTable Mod comma xs x ** {a = conjAgr xs.a x.a ; animacy = xs.animacy } ; --  InaandB xs.animacy x.animacy} ;
    ConsRS xs x = consrTable Agr comma xs x ** { c = xs.c};
    BaseAP x y = y ** twoTable Mod x y ;
    ConsAP xs x = xs ** consrTable Mod comma xs x ; -- Table3 Number Gender Case comma xs x ;-- ** {isPre = andB xs.isPre x.isPre} ;

  lincat
    [S] = {s1,s2 : VVForm => Str} ;
    [Adv] = {s1,s2 : Str} ;
    [NP] = {s1,s2 : Mod => Str ; a : Agr ; animacy : Animacy ; hasAdj : Bool} ;
    [AP] = {s1,s2 :  Mod => Str ; adv : Str ; isPre : Bool} ;
    [RS] = {s1,s2 : Agr => Str };

}
