concrete NamesChi of Names = CatChi ** {

lin GivenName, MaleSurname, FemaleSurname, PlSurname = \n -> n ;
lin FullName gn sn = {
       s = gn.s ++ sn.s
    } ;

}
