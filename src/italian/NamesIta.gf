concrete NamesIta of Names = CatIta ** open ResIta in {

lin GivenName, MaleSurname, FemaleSurname, PlSurname = \n -> pn2np n ;
lin FullName gn sn = pn2np {
       s = gn.s ++ sn.s ;
       g = gn.g
    } ;

}
