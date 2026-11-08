concrete NamesJpn of Names = CatJpn ** open ResJpn, Prelude in {

lin
  GivenName name = name ** {
    prepositive = \\st => [] ;
    needPart = True ; changePolar = False ;
    meaning = SomeoneElse
    } ;

  MaleSurname, FemaleSurname, PlSurname = \name -> name ** {
    prepositive = \\st => [] ;
    needPart = True ; changePolar = False ;
    meaning = SomeoneElse
    } ;

  FullName gn sn = {
    s = \\st => sn.s ! st ++ gn.s ! st ;
    prepositive = \\st => [] ;
    needPart = True ; changePolar = False ;
    meaning = SomeoneElse ; anim = Anim
    } ;

  UseLN, PlainLN = \name -> name ** {
    prepositive = \\st => [] ;
    needPart = True ; changePolar = False ;
    meaning = SomeoneElse
    } ;

  InLN name = {s = \\st => name.s ! st ++ "で" ; prepositive = False} ;

  AdjLN ap name = name ** {
    s = \\st => ap.attr ! st ++ name.s ! st
    } ;
}
