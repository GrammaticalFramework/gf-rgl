concrete AdjectiveFin of Adjective = CatFin ** open ResFin, StemFin, Prelude in {

  flags optimize=all_subs ; -- gfc size from 2864336 to 6786 - i.e. factor 422
    coding=utf8 ;

  lin

    PositA  a = a ** {
      s = \\_ => sAdjFull2nforms Posit a ;
      } ;
    ComparA a np =
      let acomp = sAdjFull2nforms Compar a in a ** {
      s = \\isMod,af => case isMod of {
        True => np.s ! NPCase Part ++ acomp ! af ;        -- minua isompi
        _    => acomp ! af ++ "kuin" ++ np.s ! NPSep -- isompi kuin minä
        } ;
      hasPrefix = False
      } ;
    CAdvAP ad ap np = ap ** {
      s = \\m,af => ad.s ++ ap.s ! m ! af ++ ad.p ++ np.s ! NPSep ;
      hasPrefix = False
      } ;
    UseComparA a = a ** {
      s = \\_ => sAdjFull2nforms Compar a
      } ;

-- $SuperlA$ belongs to determiner syntax in $Noun$.
    AdjOrd ord = {
      s = \\_ => ord.s ;
      p = [] ; hasPrefix = False
      } ;


    ComplA2 a np = {
      s = \\isMod,af =>
          preOrPost isMod (appCompl True Pos a.c2 np)  (sAdjFull2nforms Posit a ! af) ;
      p = [] ; hasPrefix = False
      } ;

    ReflA2 a = {
      s = \\isMod,af =>
          preOrPost isMod
            (appCompl True Pos a.c2 (reflPron (agrP3 Sg))) (sAdjFull2nforms Posit a ! af) ;
      p = [] ; hasPrefix = False
      } ;

    SentAP ap sc = ap ** {
      s = \\b,a => ap.s ! b ! a ++ sc.s ;
      hasPrefix = False
      } ;

    AdAP ada ap = ap ** {
      s = \\b,af => ada.s ++ ap.s ! b ! af ;
      hasPrefix = False
      } ;

    AdvAP ap adv = ap ** {
      s = \\b,af => adv.s ++ ap.s ! b ! af ; -- luonnostaan vaalea
      hasPrefix = False
      } ;

    UseA2 a = {
      s = \\_ => sAdjFull2nforms Posit a ;
      p = [] ; hasPrefix = False
      } ;

}
