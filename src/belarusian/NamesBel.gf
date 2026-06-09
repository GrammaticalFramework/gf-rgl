concrete NamesBel of Names = CatBel ** open ResBel, ParadigmsBel in {

lin
  GivenName gn = mkSimpleNP gn.s gn.g Sg P3 ;
  MaleSurname sn = mkSimpleNP sn.s Masc Sg P3 ;
  FemaleSurname sn = mkSimpleNP sn.s Fem Sg P3 ;
  PlSurname sn = mkSimpleNP sn.s Masc Pl P3 ;
  FullName gn sn = mkSimpleNP (gn.s ++ sn.s) gn.g Sg P3 ;

  UseLN ln = {s = ln.s; a = {g=ln.g; n=ln.n; p=P3}} ;
  PlainLN ln = {s = ln.s; a = {g=ln.g; n=ln.n; p=P3}} ;
  InLN ln = {s = "у" ++ ln.s ! Loc} ;
  AdjLN ap ln = {
    s = \\c => ap.s ! c ! genNum ln.g ln.n ++ ln.s ! c ;
    g = ln.g ;
    n = ln.n
  } ;

}
