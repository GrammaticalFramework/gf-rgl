concrete ConjunctionMay of Conjunction =
  CatMay ** open ResMay, Coordination, Prelude in {

  flags optimize=all_subs ;

  lincat
    [Adv],[AdV] = {s1,s2 : Str} ;
    [S] = {s1,s2 : Str} ;
    [AP] = {s1,s2 : Str} ;
    [NP] = {s1,s2 : Possession => Str} ;
    [CN] = {s1,s2 : NForm => Str ; heavyMod : Str} ;

  lin
    BaseAdv, BaseAdV = twoSS ;
    ConsAdv, ConsAdV = consrSS comma ;
    ConjAdv, ConjAdV = conjunctDistrSS ;

    BaseS = twoSS ;
    ConsS = consrSS comma ;
    ConjS = conjunctDistrSS ;

    BaseAP = twoSS ;
    ConsAP = consrSS comma ;
    ConjAP = conjunctDistrSS ;

    BaseNP = twoTable Possession ;
    ConsNP = consrTable Possession comma ;
    ConjNP co nps = emptyNP ** conjunctDistrTable Possession co nps ;

    BaseCN x y = y ** twoTable NForm (mergeCN x) y ;
    ConsCN x xs = xs ** consrTable NForm comma (mergeCN x) xs ;
    ConjCN conj ss = ss ** conjunctDistrTable NForm conj ss ;

  oper
    mergeCN : CNoun -> CNoun = \cn -> cn ** {s = \\nf => linCN cn} ;  -- put postmod in s field
}
