concrete NamesChi of Names = CatChi ** {

lin GivenName, MaleSurname, FemaleSurname, PlSurname = \n -> n ** {det = []} ;
lin FullName gn sn = {
       s = gn.s ++ sn.s ;
       det = []
    } ;

}
