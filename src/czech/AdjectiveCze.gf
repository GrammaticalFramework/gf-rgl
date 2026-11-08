concrete AdjectiveCze of Adjective = CatCze ** open ResCze, Prelude in {

  lin

    PositA a = let ap = adjFormsAdjective a in ap ** {pred = longPredicate ap ; isPost = False} ;

    AdAP ada ap = ap ** {
      s = \\g,n,c => ada.s ++ ap.s ! g ! n ! c ;
      pred = \\a => ada.s ++ ap.pred ! a
      } ;

    ComplA2 a np =
      let ap = adjFormsAdjective a ;
          compl = fullComplement a.c np.s np.prep
      in
      ap **  {
        s = \\g,n,c => ap.s ! g ! n ! c ++ compl ;
        pred = \\agr => longPredicate ap ! agr ++ compl ;
	isPost = True ;
	} ;

    UseA2 a = let ap = adjFormsAdjective a in ap ** {pred = longPredicate ap ; isPost = True} ;

    ReflA2 a =
      let ap = adjFormsAdjective a ;
          refl : Case => Str = table {
            Nom | ResCze.Voc => "sebe" ; Gen | Acc => "sebe" ;
            Dat | Loc => "sobě" ; Ins => "sebou"
            } ;
          compl = a.c.s ++ refl ! a.c.c
      in ap ** {
        s = \\g,n,c => ap.s ! g ! n ! c ++ compl ;
        pred = \\agr => longPredicate ap ! agr ++ compl ; isPost = True
        } ;

    UseComparA a = let ap = adjFormsAdjective a.compar in ap ** {pred = longPredicate ap ; isPost = False} ;
    ComparA a np = AdvAP (UseComparA a) {s = "než" ++ np.s ! Nom} ;
    AdjOrd ord = ord ** {pred = longPredicate ord ; isPost = False} ;

    AdvAP ap adv = ap ** {
      s = \\g,n,c => ap.s ! g ! n ! c ++ adv.s ;
      pred = \\a => ap.pred ! a ++ adv.s ; isPost = True
      } ;

    SentAP ap sc = ap ** {
      s = \\g,n,c => ap.s ! g ! n ! c ++ sc.s ;
      pred = \\a => ap.pred ! a ++ sc.s ; isPost = True
      } ;

    CAdvAP cadv ap np = ap ** {
      s = \\g,n,c => cadv.s ++ ap.s ! g ! n ! c ++ "než" ++ np.s ! Nom ;
      pred = \\a => cadv.s ++ ap.pred ! a ++ "než" ++ np.s ! Nom ;
      isPost = True
      } ;

}
