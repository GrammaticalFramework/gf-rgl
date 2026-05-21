concrete NamesHun of Names = CatHun ** open ResHun, Prelude in {

lin
  GivenName gn = gn ** {g = Human ; objdef = Def} ;
  MaleSurname sn = sn ** {g = Human ; objdef = Def} ;
  FemaleSurname sn = sn ** {g = Human ; objdef = Def} ;
  PlSurname sn = sn ** {g = Human ; objdef = Def ; agr = <P3,Pl>} ;

  FullName gn sn = emptyNP ** {
    s = \\p,c => gn.s ! p ! Nom ++ sn.s ! p ! c ;
    agr = <P3,Sg> ;
    objdef = Def ;
    g = Human ;
    postmod = [] ;
    empty = []
    } ;

  UseLN ln = ln ** {objdef = Def} ;
  PlainLN ln = ln ** {objdef = Def} ;

  InLN ln = {
    s = ln.s ! NoPoss ! Ine ;
    isPre = False
    } ;

  AdjLN ap ln = ln ** {
    s = \\p,c => ap.s ! Sg ! Nom ++ ln.s ! p ! c
    } ;

}
