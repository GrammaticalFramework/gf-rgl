
incomplete concrete ConjunctionHindustani of Conjunction = 
  CatHindustani ** open CommonHindustani, ResHindustani, Coordination, Prelude in {

--concrete ConjunctionUrd of Conjunction = 
--  CatUrd ** open ResUrd, Coordination, Prelude in {


  flags optimize=all_subs ;

  lin

    ConjS  = conjunctDistrSS  ;

    ConjAdv conj advs = conjunctDistrTable Gender conj advs ;

    ConjNP conj ss = conjunctDistrTable NPCase conj ss ** {
      a = conjAgr (agrP3 Masc conj.n) ss.a
      } ;

    ConjAP conj ss = conjunctDistrTable4 Number Gender Case Degree conj ss ; 
    ConjRS conj rs = conjunctDistrTable Agr conj rs ** { c = rs.c};

---- These fun's are generated from the list cat's.

    BaseS = twoSS ;
    ConsS = consrSS comma ;
    BaseAdv x y = twoTable Gender x y  ;
    ConsAdv xs x = consrTable Gender comma xs x ;
    BaseNP x y = twoTable NPCase x y ** {a = conjAgr x.a y.a} ;
    BaseRS x y = twoTable Agr x y ** {c = x.c};
    ConsNP xs x = consrTable NPCase comma xs x ** {a = conjAgr xs.a x.a} ;
    ConsRS xs x = consrTable Agr comma xs x ** { c = xs.c};
    BaseAP x y = twoTable4 Number Gender Case Degree x y ; -- ** {isPre = andB x.isPre y.isPre} ;
    ConsAP xs x = consrTable4 Number Gender Case Degree comma xs x ;-- ** {isPre = andB xs.isPre x.isPre} ;

  lincat
    [S] = {s1,s2 : Str} ;
    [Adv] = {s1,s2 : Gender => Str} ;
    [NP] = {s1,s2 : NPCase => Str ; a : Agr} ;
    [AP] = {s1,s2 : Number => Gender => Case => Degree => Str} ;
    [RS] = {s1,s2 : Agr => Str ; c : Case};

}
