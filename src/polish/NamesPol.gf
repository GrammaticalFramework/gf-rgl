concrete NamesPol of Names = CatPol ** { 

lin GivenName, MaleSurname, FemaleSurname = \n -> n ;
lin FullName gn sn = {
       nom = gn.nom ++ sn.nom ;
       voc = gn.nom ++ sn.voc ;
       dep = \\c => gn.nom ++ sn.dep ! c ;
       gn = gn.gn ;
       p  = gn.p
    } ;

lin UseLN n = n;

}
