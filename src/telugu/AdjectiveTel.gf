concrete AdjectiveTel of Adjective = CatTel ** open ResTel, Prelude in {

  lin

    PositA a = a ;

    UseComparA a = a ;

    ComparA a np = {
      s = \\g,n,c => np.s ! NPC Obl ++ "కంటే" ++ a.s ! g ! n ! c
      } ;

-- $SuperlA$ belongs to determiner syntax in $Noun$.

    ComplA2 a np = {
      s = \\g,n,c => np.s ! NPC Obl ++ a.c2 ++ a.s ! g ! n ! c
      } ;

    ReflA2 a = {
      s = a.s
      } ;

    SentAP ap sc = {
      s = \\g,n,c => sc.s ++ ap.s ! g ! n ! c
      } ;

    AdAP ada ap = {
      s = \\g,n,c => ada.s ++ ap.s ! g ! n ! c
      } ;

    AdvAP ap adv = {
      s = \\g,n,c => adv.s ++ ap.s ! g ! n ! c
      } ;

    UseA2 a = a ;

}
