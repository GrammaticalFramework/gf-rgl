concrete NamesFin of Names = CatFin ** open ResFin, StemFin, Prelude in {

lin GivenName, MaleSurname, FemaleSurname = \n -> {
      s = snoun2np Sg n ;
      a = agrP3 Sg ;
      isPron = False ; isNeg = False
      } ;
lin FullName gn sn = {
      s = snoun2np Sg {s = \\c => gn.s ! Nom ++ sn.s ! c} ;
      a = agrP3 Sg ;
      isPron = False ; isNeg = False
      } ;

lin UseLN pn = {
      s = snoun2np Sg pn ;
      a = agrP3 Sg ;
      isPron = False ; isNeg = False
      } ;

}
