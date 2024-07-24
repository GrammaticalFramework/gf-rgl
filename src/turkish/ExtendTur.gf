concrete ExtendTur of Extend = CatTur ** open ResTur in {

  lin
    GenModNP num np cn = {
      s = \\c => np.s ! Nom ++ num.s ! num.n ! c ++ cn.gen ! num.n ! np.a ;
      h = cn.h ;
      a = {n=num.n; p=P3} ;
    } ;

    UttVPShort vp = {s = vp.s ! VInf Pos} ;

    TPastSimple = {s = []} ** {t = Past} ;  --# notpresent

    PositAdVAdj a = {s = a.s ! Sg ! Nom} ;

}
