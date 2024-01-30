concrete NamesRus of Names = CatRus ** open ResRus, MorphoRus, Prelude in {

lin GivenName gn =
      { s=gn.s ;
        pron=False;
        a=let g = case gn.g of {
                    Male => Masc ;
                    Female => Fem
                  }
          in Ag (gennum g Sg) P3
      } ;   -- Does NP need animacy?
lin MaleSurname sn =
      { s=\\cas => sn.s ! Male ! cas ;
        pron=False;
        a=Ag (GSg Masc) P3
      } ;   -- Does NP need animacy?
lin FemaleSurname sn =
      { s=\\cas => sn.s ! Female ! cas ;
        pron=False;
        a=Ag (GSg Fem) P3
      } ;   -- Does NP need animacy?
lin PlSurname sn =
      { s=sn.p ;
        pron=False;
        a=Ag GPl P3
      } ;   -- Does NP need animacy?

lin FullName gn sn =
      { s=\\cas => gn.s ! cas ++ sn.s ! gn.g ! cas ;
        pron=False;
        a=let g = case gn.g of {
                    Male => Masc ;
                    Female => Fem
                  }
          in Ag (GSg g) P3
      } ;

  UseLN, PlainLN = \ln -> {
    s=\\cas => ln.s ! cas ;
    pron=False;
    a=Ag (gennum ln.g ln.n) P3
    } ;   -- Does NP need animacy?

  InLN ln = ss (applyPrep ln.c {
    s=ln.s ;
    pron=False;
    a=Ag (gennum ln.g ln.n) P3
    }) ;   -- Does NP need animacy?

  AdjLN ap ln = ln ** {
    s=\\cas => preOrPost (notB ap.isPost) (ap.s ! (gennum ln.g ln.n) ! ln.anim ! cas) (ln.s ! cas)
    } ;

}
