concrete NamesAfr of Names = CatAfr ** open ResAfr, Prelude in {

lin GivenName, MaleSurname, FemaleSurname = \n -> {s = n.s ; a = agrP3 Sg ; isPron = False} ;
lin FullName gn sn =
      {s = \\c => gn.s ! NPNom ++ sn.s ! c ; a = agrP3 Sg ; isPron = False} ;
}
