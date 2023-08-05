concrete NamesSlv of Names = CatSlv ** open ResSlv, Prelude in {

lin GivenName = \n -> {
      s = n.s;
      a = {g=agender2gender (sex2agender n.g); n=Sg; p=P3};
      isPron = False
      } ;
lin MaleSurname = \n -> {
      s = n.s ! Male;
      a = {g=Masc; n=Sg; p=P3};
      isPron = False
      } ;
lin FemaleSurname = \n -> {
      s = n.s ! Female;
      a = {g=Fem; n=Sg; p=P3};
      isPron = False
      } ;
lin FullName gn sn = {
      s = \\c => gn.s ! Nom ++ sn.s ! gn.g ! c ;
      a = {g=agender2gender (sex2agender gn.g); n=Sg; p=P3};
      isPron = False
      } ;

}
