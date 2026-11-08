concrete NamesMon of Names = CatMon ** open Prelude, ResMon in {

  oper
    nameNP : (RCase => Str) -> NP = \s -> lin NP {
      s = s ;
      n = Sg ; p = P3 ; isPron = False ; isDef = True
      } ;

  lin
    UseLN ln = nameNP ln.s ;
    InLN ln = {s = ln.s ! Dat} ;
    AdjLN ap ln = {s = \\rc => ap.s ++ ln.s ! rc} ;
    GivenName gn = nameNP gn.s ;
    MaleSurname sn = nameNP sn.s ;
}
