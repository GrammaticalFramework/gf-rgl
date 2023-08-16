concrete NamesAfr of Names = CatAfr ** open ResAfr, Prelude in {

lin GivenName = \n -> {s = n.s ; a = agrP3 Sg ; isPron = False} ;
lin MaleSurname = \n -> {s = n.s ! Male ; a = agrP3 Sg ; isPron = False} ;
lin FemaleSurname = \n -> {s = n.s ! Female; a = agrP3 Sg ; isPron = False} ;
lin PlSurname = \n -> {s = n.pl ; a = agrP3 Sg ; isPron = False} ;

lin FullName gn sn =
      {s = \\c => gn.s ! NPNom ++ sn.s ! gn.g ! c ; a = agrP3 Sg ; isPron = False} ;

lin UseLN ln = {
      s = \\c => case ln.hasArt of {
                   True  => "die" ++ ln.s ! Weak ! c ;
                   False => ln.s ! Strong ! c
                 } ;
      a = agrP3 ln.n ;
      isPron = False
    } ;

   PlainLN ln = {
      s = \\c => ln.s ! Strong ! c ;
      a = agrP3 ln.n ;
      isPron = False
    } ;

   InLN ln = {
      s = appPrep "in" (\\c => case ln.hasArt of {
                                 True  => "die" ++ ln.s ! Weak ! c ;
                                 False => ln.s ! Strong ! c
                               })
    } ;

   AdjLN ap ln = ln ** {
      s = \\a,c =>
               preOrPost ap.isPre
                 (ap.s ! agrAdj Neutr a (NF ln.n Nom))
                 (ln.s ! a ! c) ;
      } ;

}
