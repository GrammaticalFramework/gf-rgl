concrete NamesKor of Names = CatKor ** open ResKor in {

lin GivenName, MaleSurname, FemaleSurname = \n -> n ;
lin FullName gn sn = {
       s = \\nf => gn.s ! nf ++ sn.s ! nf ;
       p = gn.p
    } ;

  UseLN pn = pn ;

}
