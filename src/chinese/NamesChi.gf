concrete NamesChi of Names = CatChi ** open ResChi, ParadigmsChi, Prelude in {

lin GivenName, MaleSurname, FemaleSurname, PlSurname = \n -> n ** {det = []} ;
lin FullName gn sn = {
       s = gn.s ++ sn.s ;
       det = []
    } ;

lin UseLN ln = ln ** {det = []} ;

lin InLN ln = 
  let prep : Prep = mkPrep "里" []
  in ss (appPrep prep (linNP (ln ** {det = []}))) ** {advType = prep.advType ; hasDe = prep.hasDe} ; --- should depend on np too ?

}
