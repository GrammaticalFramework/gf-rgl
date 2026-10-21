
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
    ConjCN conj cns = conjunctDistrTable2 Number Case conj cns ** {g = cns.g} ;
    ConjAdV conj xs = conjunctDistrSS conj xs ;
    ConjIAdv conj xs = conjunctDistrSS conj xs ;
    ConjDet conj xs = conjunctDistrTable3 Number Gender Case conj xs ** {n = conj.n} ;
    ConjDAP conj xs = conjunctDistrTable3 Number Gender Case conj xs ** {n = conj.n} ;

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
    BaseCN x y = twoTable2 Number Case x y ** {g = x.g} ;
    ConsCN xs x = consrTable2 Number Case comma xs x ** {g = xs.g} ;
    BaseAdV = twoSS ;
    ConsAdV = consrSS comma ;
    BaseIAdv = twoSS ;
    ConsIAdv = consrSS comma ;
    BaseDet x y = twoTable3 Number Gender Case x y ** {n = conjNumber x.n y.n} ;
    ConsDet xs x = consrTable3 Number Gender Case comma xs x ** {n = conjNumber xs.n x.n} ;
    BaseDAP x y = twoTable3 Number Gender Case x y ** {n = conjNumber x.n y.n} ;
    ConsDAP xs x = consrTable3 Number Gender Case comma xs x ** {n = conjNumber xs.n x.n} ;

  lincat
    [S] = {s1,s2 : Str} ;
    [Adv] = {s1,s2 : Gender => Str} ;
    [NP] = {s1,s2 : NPCase => Str ; a : Agr} ;
    [AP] = {s1,s2 : Number => Gender => Case => Degree => Str} ;
    [RS] = {s1,s2 : Agr => Str ; c : Case};
    [CN] = {s1,s2 : Number => Case => Str ; g : Gender} ;
    [AdV], [IAdv] = {s1,s2 : Str} ;
    [Det], [DAP] = {s1,s2 : Number => Gender => Case => Str ; n : Number} ;

}
