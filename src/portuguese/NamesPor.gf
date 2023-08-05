concrete NamesPor of Names = CatPor ** open ResPor in {

lin GivenName, MaleSurname, FemaleSurname = \n -> pn2np n ;
lin FullName gn sn = pn2np {
       s = gn.s ++ sn.s ;
       g = gn.g
    } ;

}
