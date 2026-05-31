concrete NamesFao of Names = CatFao ** open ResFao in {

lin
  GivenName gn = mkNP gn.s Masc Sg P3 ;
  MaleSurname sn = mkNP sn.s Masc Sg P3 ;
  FemaleSurname sn = mkNP sn.s Fem Sg P3 ;
  PlSurname sn = mkNP sn.s Masc Pl P3 ;
  FullName gn sn = mkNP (gn.s ++ sn.s) Masc Sg P3 ;

  UseLN ln = mkNP ln.s Neuter Sg P3 ;
  PlainLN ln = mkNP ln.s Neuter Sg P3 ;
  InLN ln = {s = "í" ++ ln.s} ;
  AdjLN ap ln = {s = ap.s ! Neuter ! Sg ! Nom ++ ln.s} ;
}
