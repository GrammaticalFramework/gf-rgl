concrete NamesEus of Names = CatEus ** open ResEus, Prelude in {

  lin
    GivenName = nameNP ;
    MaleSurname = nameNP ;
    FemaleSurname = nameNP ;
    PlSurname sn = (nameNP sn) ** {agr = Hauek} ;

    FullName gn sn = nameNP (sn ** {s = gn.s ++ sn.s ; nbr = Sg}) ;

    UseLN, PlainLN = nameNP ;

    InLN ln = {s = (nameNP ln).s ! Ine} ;

    AdjLN ap ln = ln ** {s = ap.s ! Hau ++ ln.s} ;

  oper
    nameNP : PNoun -> NounPhrase = \pn -> {
      s = \\c => pn.s ++ artIndef ! c ! pn.ph ;
      stem = pn.s ;
      agr = case pn.nbr of {Sg => Hau ; Pl => Hauek} ;
      anim = pn.anim ;
      isDef = True
      } ;
}
