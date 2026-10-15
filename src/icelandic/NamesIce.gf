concrete NamesIce of Names = CatIce ** open ResIce, Prelude in {

lin
  GivenName gn = nameNP gn.s Masc Sg ;
  MaleSurname sn = nameNP sn.s Masc Sg ;
  FemaleSurname sn = nameNP sn.s Fem Sg ;
  PlSurname sn = nameNP sn.s Masc Pl ;
  FullName gn sn = nameNP (gn.s ++ sn.s) Masc Sg ;

  UseLN ln = nameNP ln.s Neutr Sg ;
  PlainLN ln = nameNP ln.s Neutr Sg ;
  InLN ln = {s = "í" ++ ln.s} ;
  AdjLN ap ln = {s = ap.s ! Sg ! Neutr ! Strong ! Nom ++ ln.s} ;

oper
  nameNP : Str -> Gender -> Number -> ResIce.NP = \s,g,n -> {
    s = \\_ => s ;
    a = gennumperToAgr g n P3 ;
    isPron = False
  } ;
}
