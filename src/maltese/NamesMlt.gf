concrete NamesMlt of Names = CatMlt ** open ResMlt, Prelude in { 

lin GivenName, MaleSurname, FemaleSurname, PlSurname = \n -> {
      s = \\c => n.s ;
      a = n.a ;
      isPron = False ;
      isDefn = False
      } ;
lin FullName gn sn = {
      s = \\c => gn.s ++ sn.s ;
      a = gn.a ;
      isPron = False ;
      isDefn = False
      } ;

lin UseLN pn = {
      s = \\c => pn.s ;
      a = pn.a ;
      isPron = False ;
      isDefn = False ;
      } ;

}
