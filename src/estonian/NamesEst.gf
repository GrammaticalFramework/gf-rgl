concrete NamesEst of Names = CatEst ** open ResEst, Prelude in {

lin GivenName, MaleSurname, FemaleSurname = \n -> emptyNP ** {
      s = \\c => n.s ! npform2case Sg c ;
      a = agrP3 Sg ;
      isPron = False
      } ;
lin FullName gn sn = emptyNP ** {
      s = \\c => gn.s ! Nom ++ sn.s ! npform2case Sg c ;
      a = agrP3 Sg ;
      isPron = False
      } ;

lin UseLN pn = emptyNP ** {
      s = \\c => pn.s ! npform2case Sg c ;
      a = agrP3 Sg ;
      isPron = False
      } ;

}
