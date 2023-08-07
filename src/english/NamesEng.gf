concrete NamesEng of Names = CatEng ** open Prelude, ResEng in {

lin GivenName gn = {s = \\c => gn.s ! npcase2case c ; a = agrgP3 Sg gn.g} ;
lin MaleSurname, FemaleSurname = \sn -> {s = \\c => sn.s ! npcase2case c ; a = agrgP3 Sg sn.g} ;
lin FullName gn sn = {s = \\c => gn.s ! Nom ++ sn.s ! npcase2case c ; a = agrgP3 Sg gn.g} ;

lin UseLN n = {
      s = \\c => case n.art of {
                   True  => "the" ++ n.s ! npcase2case c ;
                   False => n.s ! npcase2case c
                 } ;
      a = agrP3 n.n
    } ;

lin PlainLN n = {
      s = \\c => n.s ! npcase2case c ;
      a = agrP3 n.n
    } ;

lin InLN n = {
      s = n.p ++ case n.art of {
                   True  => "the" ++ n.s ! Nom ;
                   False => n.s ! Nom
                 } ;
    } ;
    
lin AdjLN ap n = n ** {
      s = \\c => preOrPost ap.isPre (ap.s ! agrP3 n.n) (n.s ! c) ;
    } ;

}
