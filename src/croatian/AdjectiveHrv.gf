concrete AdjectiveHrv of Adjective = CatHrv ** open ResHrv, Prelude in {

  lin

    PositA a = adjFormsAdjective a.posit ** {isPost = False} ;
    
    AdAP ada ap = ap ** {s = \\g,n,c => ada.s ++ ap.s ! g ! n ! c} ;

    AdjOrd a = adjFormsAdjective a ** {isPost = False} ;

    ComparA a np =
      let ap = adjFormsAdjective a.compar
      in
      ap **  {
        s = \\g,n,c => ap.s ! g ! n ! c ++ od_Str ++ np.s ! Gen ;
	isPost = True ;
	} ;
	
    ComplA2 a np =
      let ap = adjFormsAdjective a
      in
      ap **  {
        s = \\g,n,c => ap.s ! g ! n ! c ++ a.c.s ++ np.s ! a.c.c ;
	isPost = True ;
	} ;

    UseA2 a = adjFormsAdjective a ** {isPost = False} ;

    UseComparA a = adjFormsAdjective a.compar ** {isPost = False} ;

    AdvAP ap adv = ap ** {s = \\g,n,c => ap.s ! g ! n ! c ++ adv.s ; isPost = True} ;

}
