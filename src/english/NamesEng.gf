concrete NamesEng of Names = CatEng ** open Prelude, ResEng in {

lin GivenName gn = {
      s = \\c => gn.s ! npcase2case c ;
      a = case gn.g of {
            Male   => agrgP3 Sg Masc ;
            Female => agrgP3 Sg Fem
          }
      } ;
lin MaleSurname = \sn -> {s = \\c => sn.s ! Male ! npcase2case c ; a = agrgP3 Sg Masc} ;
lin FemaleSurname = \sn -> {s = \\c => sn.s ! Female ! npcase2case c ; a = agrgP3 Sg Fem} ;
lin PlSurname = \sn -> {s = \\c => sn.p ! npcase2case c ; a = agrgP3 Pl Masc} ;
lin FullName gn sn = {
      s = \\c => gn.s ! Nom ++ sn.s ! gn.g ! npcase2case c ;
      a = case gn.g of {
            Male   => agrgP3 Sg Masc ;
            Female => agrgP3 Sg Fem
          }
      } ;

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
      s = case n.prep of {
            InPrep => "in" ;
            OnPrep => "on" ;
            AtPrep => "at"
          } ++
          case n.art of {
            True  => "the" ++ n.s ! Nom ;
            False => n.s ! Nom
          } ;
    } ;
    
lin AdjLN ap n = n ** {
      s = \\c => preOrPost ap.isPre (ap.s ! agrP3 n.n) (n.s ! c) ;
    } ;

}
