concrete NamesSom of Names = CatSom ** open ResSom, Prelude in {

lin GivenName, MaleSurname, FemaleSurname = \n -> n ** {
    s = \\c => n.s ;
    isPron = False ;
    st = Definite ;
    empty = [] ;
    };

lin FullName gn sn = {
       s = \\c => gn.s ++ sn.s ;
       a = gn.a ;
       isPron = False ;
       st = Definite ;
       empty = [] ;
    } ;

  UseLN pn = pn ** {
    s = \\c => pn.s ;
    isPron = False ;
    st = Definite ;
    empty = [] ;
    } ;

}
