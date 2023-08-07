concrete NamesDut of Names = CatDut ** open Prelude, ResDut in {

lin GivenName, MaleSurname, FemaleSurname = \n -> noMerge ** {s = n.s ; a = agrP3 Sg ; isPron = False} ;
lin FullName gn sn =
       noMerge ** {s = \\c => gn.s ! NPNom ++ sn.s ! c ; a = agrP3 Sg ; isPron = False} ;

lin UseLN pn = noMerge ** {s = pn.s ; a = agrP3 Sg ; isPron = False} ;

}
