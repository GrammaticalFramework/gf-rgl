concrete NamesLav of Names = CatLav ** open Prelude, ResLav in {

lin
  GivenName gn = {
    s = gn.s ;
    agr    = AgrP3 Sg gn.gend ;
    pol    = Pos ;
    isRel  = False ;
    isPron = False
  } ;
  MaleSurname sn = {
    s = sn.s ! Masc ;
    agr    = AgrP3 Sg Masc ;
    pol    = Pos ;
    isRel  = False ;
    isPron = False
  } ;
  FemaleSurname sn = {
    s = sn.s ! Fem ;
    agr    = AgrP3 Sg Fem ;
    pol    = Pos ;
    isRel  = False ;
    isPron = False
  } ;
  PlSurname sn = {
    s = sn.pl ;
    agr    = AgrP3 Pl Masc ;
    pol    = Pos ;
    isRel  = False ;
    isPron = False
  } ;
  FullName gn sn = {
    s = \\c => gn.s ! c ++ sn.s ! gn.gend ! c ;
    agr    = AgrP3 Sg gn.gend ;
    pol    = Pos ;
    isRel  = False ;
    isPron = False
  } ;

lin
  UseLN, PlainLN = \ln -> {
    s      = ln.s ;
    agr    = AgrP3 ln.num ln.gend ;
    pol    = Pos ;
    isRel  = False ;
    isPron = False
  } ;

}
