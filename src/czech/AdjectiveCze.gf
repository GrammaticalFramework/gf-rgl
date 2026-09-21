concrete AdjectiveCze of Adjective = CatCze ** open ResCze, Prelude in {

  lin

    PositA a = let ap = adjFormsAdjective a in ap ** {pred = longPredicate ap ; isPost = False} ;

    AdAP ada ap = ap ** {
      s = \\g,n,c => ada.s ++ ap.s ! g ! n ! c ;
      pred = \\a => ada.s ++ ap.pred ! a
      } ;

    ComplA2 a np =
      let ap = adjFormsAdjective a ;
          compl = a.c.s ++ case a.c.hasPrep of {
            True => np.prep ! a.c.c ; False => np.s ! a.c.c
            }
      in
      ap **  {
        s = \\g,n,c => ap.s ! g ! n ! c ++ compl ;
        pred = \\agr => longPredicate ap ! agr ++ compl ;
	isPost = True ;
	} ;

    UseA2 a = let ap = adjFormsAdjective a in ap ** {pred = longPredicate ap ; isPost = True} ;

    UseComparA a = let ap = adjFormsAdjective a.compar in ap ** {pred = longPredicate ap ; isPost = False} ;
    AdjOrd ord = ord ** {pred = longPredicate ord ; isPost = False} ;

    AdvAP ap adv = ap ** {
      s = \\g,n,c => ap.s ! g ! n ! c ++ adv.s ;
      pred = \\a => ap.pred ! a ++ adv.s ; isPost = True
      } ;

}
