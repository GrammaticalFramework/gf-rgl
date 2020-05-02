concrete AdjectiveSlo of Adjective = CatSlo ** open ResSlo, Prelude in {

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

    UseComparA a = adjFormsAdjective a ** {isPost = False} ; ---- TODO: this gives positive forms 

    AdvAP ap adv = ap ** {s = \\g,n,c => ap.s ! g ! n ! c ++ adv.s ; isPost = True} ;

}
