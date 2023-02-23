concrete ConjunctionMay of Conjunction =
  CatMay ** open ResMay, Coordination, Prelude in {

  flags optimize=all_subs ;

  lincat
    [Adv],[AdV] = {s1,s2 : Str} ;
    [S] = {s1,s2 : Str} ;


  lin
    BaseAdv, BaseAdV = twoSS ;
    ConsAdv, ConsAdV = consrSS comma ;
    ConjAdv, ConjAdV = conjunctDistrSS ;
    BaseS = twoSS ;
    ConsS = consrSS comma ;
    ConjS = conjunctDistrSS ;

}
