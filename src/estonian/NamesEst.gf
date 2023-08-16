concrete NamesEst of Names = CatEst ** open ResEst, ParadigmsEst, Prelude in {

lin GivenName n = emptyNP ** {
      s = \\c => n.s ! npform2case Sg c ;
      a = agrP3 Sg ;
      isPron = False
      } ;
lin MaleSurname n = emptyNP ** {
      s = \\c => n.s ! Male ! npform2case Sg c ;
      a = agrP3 Sg ;
      isPron = False
      } ;
lin FemaleSurname n = emptyNP ** {
      s = \\c => n.s ! Female ! npform2case Sg c ;
      a = agrP3 Sg ;
      isPron = False
      } ;
lin PlSurname n = emptyNP ** {
      s = \\c => n.pl ! npform2case Sg c ;
      a = agrP3 Pl ;
      isPron = False
      } ;
lin FullName gn sn = emptyNP ** {
      s = \\c => gn.s ! Nom ++ sn.s ! gn.g ! npform2case Sg c ;
      a = agrP3 Sg ;
      isPron = False
      } ;

lin UseLN, PlainLN = \ln -> emptyNP ** {
      s = \\c => ln.s ! npform2case Sg c ;
      a = agrP3 Sg ;
      isPron = False
      } ;

lin InLN pn = {
      s = appCompl True Pos (casePrep inessive)
                            (emptyNP ** {
                               s = \\c => pn.s ! npform2case Sg c ;
                               a = agrP3 Sg ;
                               isPron = False
                             })
      } ;

}
