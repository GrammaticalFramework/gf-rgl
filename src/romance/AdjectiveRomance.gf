incomplete concrete AdjectiveRomance of Adjective =
  CatRomance ** open CommonRomance, ResRomance, Prelude in {
  flags coding=utf8;
  lin

    PositA a = a ; -- A is a subtype of AP: we lose the fields isDeg and compar
    ComparA a np = a ** {
      s = \\af =>
        let compar : Str = case a.isDeg of {
                    True => a.compar ! af2compar af ;   -- bueno, mejor
                    False => piuComp ++ a.s ! af } ; -- cher, plus cher
        in compar ++ conjThan ++ (np.s ! Nom).ton ;
      isPre = False ;
      } ;
    CAdvAP ad ap np = ap ** {
      s = \\af => ad.s ++ ap.s ! af ++ ad.p ++ (np.s ! Nom).ton ;
      isPre = False ;
      } ;
    UseComparA a = a ** {
      s = \\af => case a.isDeg of {
                    True => a.compar ! af2compar af ;
                    False => piuComp ++ a.s ! af }
      } ;
    AdjOrd ord = {
      s = \\af => ord.s ! aform2aagr af ; ----
      isPre = False ; ----
      copTyp = serCopula
      } ;

-- $SuperlA$ belongs to determiner syntax in $Noun$.

    ComplA2 adj np = adj ** {
      s = \\af => adj.s ! af ++ appCompl adj.c2 np ;
      isPre = False ;
      } ;

    ReflA2 adj = adj ** {
      s = \\af =>
             adj.s ! af ++
             adj.c2.s ++ prepCase adj.c2.c ++ reflPron Sg P3 Nom ; --- agr
      isPre = False ;
      } ;

    SentAP ap sc = ap ** {
      s = \\a => ap.s ! a ++ sc.s ! dative ; -- prête à dormir --- mood
      isPre = False ;
      } ;

    AdAP ada ap = ap ** {
      s = \\a => ada.s ++ ap.s ! a ;
      } ;

    UseA2 a = a ** {
      isPre = False ; ---- A2 has no isPre
      } ;             -- other than that, AP is a subtype of A2

    AdvAP ap adv = ap ** {
      s = \\a => ap.s ! a ++ adv.s ;
      isPre = False ;
      } ;


}
