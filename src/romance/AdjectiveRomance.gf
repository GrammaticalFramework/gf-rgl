incomplete concrete AdjectiveRomance of Adjective =
  CatRomance ** open CommonRomance, ResRomance, Prelude in {
  flags coding=utf8;
  lin

    PositA a = a ** {
      s = a.s ! Posit ;
      } ;
    ComparA a np = a ** {
      s = \\af =>
        let compar : Str = case a.isDeg of {
          True => a.s ! Compar ! af ;        -- bueno, mejor
          False => piuComp ++ a.s ! Posit ! af } ; -- cher, plus cher
        in compar ++ conjThan ++ (np.s ! Nom).ton ;
      isPre = False ;
      } ;
    CAdvAP ad ap np = {
      s = \\af => ad.s ++ ap.s ! af ++ ad.p ++ (np.s ! Nom).ton ;
      isPre = False ;
      copTyp = ap.copTyp
      } ;
    UseComparA a = a ** {
      s = \\af => a.s ! Compar ! af ;
      } ;
    AdjOrd ord = {
      s = \\af => ord.s ! aform2aagr af ; ----
      isPre = False ; ----
      copTyp = serCopula
      } ;

-- $SuperlA$ belongs to determiner syntax in $Noun$.

    ComplA2 adj np = {
      s = \\af => adj.s ! Posit ! af ++ appCompl adj.c2 np ;
      isPre = False ;
      copTyp = adj.copTyp
      } ;

    ReflA2 adj = {
      s = \\af =>
             adj.s ! Posit ! af ++
             adj.c2.s ++ prepCase adj.c2.c ++ reflPron Sg P3 Nom ; --- agr
      isPre = False ;
      copTyp = adj.copTyp
      } ;

    SentAP ap sc = {
      s = \\a => ap.s ! a ++ sc.s ! dative ; -- prête à dormir --- mood
      isPre = False ;
      copTyp = ap.copTyp
      } ;

    AdAP ada ap = {
      s = \\a => ada.s ++ ap.s ! a ;
      isPre = ap.isPre ;
      copTyp = ap.copTyp
      } ;

    UseA2 a = {
      s = a.s ! Posit ;
      isPre = False ; ---- A2 has no isPre
      copTyp = a.copTyp
      } ;

    AdvAP ap adv = {
      s = \\a => ap.s ! a ++ adv.s ;
      isPre = False ;
      copTyp = ap.copTyp
      } ;


}
