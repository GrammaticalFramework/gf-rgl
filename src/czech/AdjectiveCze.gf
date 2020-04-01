concrete AdjectiveCze of Adjective = CatCze ** open ResCze, Prelude in {

  lin

    PositA a = adjFormsAdjective a ** {isPost = False} ;
    
    AdAP ada ap = ap ** {s = \\g,n,c => ada.s ++ ap.s ! g ! n ! c} ;
    
    ComplA2 a np =
      let ap = adjFormsAdjective a
      in
      ap **  {
        s = \\g,n,c => ap.s ! g ! n ! c ++ a.c.s ++ np.s ! a.c.c ;
	isPost = True ;
	} ;

    UseA2 a = adjFormsAdjective a ** {isPost = False} ;

}
