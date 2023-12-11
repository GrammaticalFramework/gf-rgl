concrete NamesSlv of Names = CatSlv ** open ResSlv, (P=ParamX), Prelude in {

lin GivenName = \n -> {
      s = n.s;
      a = {g=agender2gender (sex2agender n.g); n=Sg; p=P3};
      isPron = False
      } ;
lin MaleSurname = \n -> {
      s = n.s ! P.Male;
      a = {g=Masc; n=Sg; p=P3};
      isPron = False
      } ;
lin FemaleSurname = \n -> {
      s = n.s ! P.Female;
      a = {g=Fem; n=Sg; p=P3};
      isPron = False
      } ;
lin FullName gn sn = {
      s = \\c => gn.s ! Nom ++ sn.s ! gn.g ! c ;
      a = {g=agender2gender (sex2agender gn.g); n=Sg; p=P3};
      isPron = False
      } ;

lin UseLN, PlainLN = \ln -> {
      s = ln.s;
      a = {g=agender2gender ln.g; n=ln.n; p=P3};
      isPron = False
      } ;

lin InLN ln = {
      s = "v" ++ ln.s ! Loc
      } ;

}
