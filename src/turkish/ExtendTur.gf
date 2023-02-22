concrete ExtendTur of Extend = CatTur ** open ResTur in {

  lin
    GenModNP num np cn = {
      s = \\c => np.s ! Nom ++ num.s ! num.n ! c ++ cn.gen ! num.n ! np.a ;
      h = cn.h ;
      a = {n=num.n; p=P3} ;
    } ;

lin GivenName, MaleSurname, FemaleSurname = \n -> n ** {n = Sg};
lin PlSurname = \n -> n ** {n = Pl};
lin FullName gn sn = {
       s = \\c => gn.s ! Nom ++ sn.s ! c ;
       h = sn.h ;
       n = Sg
    } ;

}
