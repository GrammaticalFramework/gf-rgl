incomplete concrete AdjectiveRomance of Adjective =
  CatRomance ** open CommonRomance, ResRomance, Prelude in {
  flags coding=utf8;
  lin

    PositA a = {
      s = \\_p => a.s ! Posit ;
      isPre = a.isPre ;
      copTyp = a.copTyp        
      } ;
    ComparA a np = {
      s = \\_p,af => a.s ! Compar ! af ++ conjThan ++ (np.s ! Nom).ton ;
      isPre = False ;
      copTyp = a.copTyp
      } ;
    CAdvAP ad ap np = {
      s = \\_p,af => ad.s ++ ap.s ! _p ! af ++ ad.p ++ (np.s ! Nom).ton ;
      isPre = False ;
      copTyp = ap.copTyp
      } ;
    UseComparA a = {
      s = \\_p,af => a.s ! Compar ! af ;
      isPre = a.isPre ;
      copTyp = a.copTyp
      } ;
    AdjOrd ord = {
      s = \\_p,af => ord.s ! aform2aagr af ; ----
      isPre = False ; ----
      copTyp = serCopula
      } ;

-- $SuperlA$ belongs to determiner syntax in $Noun$.

    ComplA2 adj np = {
      s = \\_p,af => adj.s ! Posit ! af ++ appCompl adj.c2 np ;
      isPre = False ;
      copTyp = serCopula
      } ;

    ReflA2 adj = {
      s = \\p,af =>
             adj.s ! Posit ! af ++
             adj.c2.s ++ prepCase adj.c2.c ++ reflPron Sg p Nom ; --- agr
      isPre = False ;
      copTyp = serCopula
      } ;

    SentAP ap sc = {
      s = \\_p,a => ap.s ! _p ! a ++ sc.s ! dative ; -- prête à dormir --- mood
      isPre = False ;
      copTyp = ap.copTyp
      } ;

    AdAP ada ap = {
      s = \\_p,a => ada.s ++ ap.s ! _p ! a ;
      isPre = ap.isPre ;
      copTyp = ap.copTyp
      } ;

    UseA2 a = {
      s = \\_p => a.s ! Posit ;
      isPre = False ; ---- A2 has no isPre
      copTyp = serCopula ---- A2 has no copTyp (yet)
      } ;

    AdvAP ap adv = {
      s = \\_p,a => ap.s ! _p ! a ++ adv.s ;
      isPre = False ;
      copTyp = ap.copTyp
      } ;


}
