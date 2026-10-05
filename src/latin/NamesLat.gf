concrete NamesLat of Names = CatLat ** open Prelude, ResLat in {

  lin
    GivenName name = dummyNP name.s ** {
      g = case name.g of {Male => Masc; Female => Fem}; n = Sg
      } ;
    MaleSurname name = dummyNP (name.s ! Male) ** {g = Masc; n = Sg} ;
    FemaleSurname name = dummyNP (name.s ! Female) ** {g = Fem; n = Sg} ;
    PlSurname name = dummyNP name.pl ** {g = Masc; n = Pl} ;
    FullName first last = dummyNP (first.s ++ last.s ! first.g) ** {
      g = case first.g of {Male => Masc; Female => Fem}; n = Sg
      } ;

    UseLN name = {
      s = \\_,c => name.s ! c; n = name.n; g = name.g; p = P3; adv = "";
      preap, postap = {s = \\_ => ""}; det = {s,sp = \\_ => ""; n = name.n}
      } ;
    PlainLN name = {
      s = \\_,c => name.s ! c; n = name.n; g = name.g; p = P3; adv = "";
      preap, postap = {s = \\_ => ""}; det = {s,sp = \\_ => ""; n = name.n}
      } ;
    InLN name = mkAdverb ("in" ++ name.s ! Abl) ;
    AdjLN ap name = name ** {
      s = \\c => name.s ! c ++ ap.s ! Ag name.g name.n c
      } ;

}
