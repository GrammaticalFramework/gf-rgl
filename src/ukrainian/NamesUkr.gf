concrete NamesUkr of Names = CatUkr ** open ResUkr in {

lin
  GivenName gn = {s = \\_ => gn.s ; g = Masc ; n = Sg ; p = P3} ;
  MaleSurname sn = {s = \\_ => sn.s ; g = Masc ; n = Sg ; p = P3} ;
  FemaleSurname sn = {s = \\_ => sn.s ; g = Fem ; n = Sg ; p = P3} ;
  PlSurname sn = {s = \\_ => sn.s ; g = Masc ; n = Pl ; p = P3} ;
  FullName gn sn = {s = \\_ => gn.s ++ sn.s ; g = Masc ; n = Sg ; p = P3} ;

  UseLN ln = {s = \\_ => ln.s ; g = Fem ; n = Sg ; p = P3} ;
  PlainLN ln = UseLN ln ;
  InLN ln = {s = "у" ++ ln.s} ;
  AdjLN ap ln = {s = ap.s ! Nom ! (GSg Fem) ++ ln.s} ;
}
