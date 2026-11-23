concrete AdjectiveTel of Adjective = CatTel ** open ResTel, Prelude in {

  lin

    PositA a = a ;

    UseComparA a = a ;

    ComparA a np = {
      s = np.s ! NPC Obl ++ "కంటే" ++ a.s ;
      pred = \\g,n,c => np.s ! NPC Obl ++ "కంటే" ++ a.pred ! g ! n ! c
      } ;

-- $SuperlA$ belongs to determiner syntax in $Noun$.

    ComplA2 a np = {
      s = np.s ! NPC Obl ++ a.c2 ++ a.s ;
      pred = \\g,n,c => np.s ! NPC Obl ++ a.c2 ++ a.pred ! g ! n ! c
      } ;

    ReflA2 a = {
      s = a.s ;
      pred = a.pred
      } ;

    SentAP ap sc = {
      s = sc.s ++ ap.s ;
      pred = \\g,n,c => sc.s ++ ap.pred ! g ! n ! c
      } ;

    AdAP ada ap = {
      s = ada.s ++ ap.s ;
      pred = \\g,n,c => ada.s ++ ap.pred ! g ! n ! c
      } ;

    AdvAP ap adv = {
      s = adv.s ++ ap.s ;
      pred = \\g,n,c => adv.s ++ ap.pred ! g ! n ! c
      } ;

    UseA2 a = a ;

}
