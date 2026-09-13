concrete AdjectiveTel of Adjective = CatTel ** open ResTel, Prelude in {

  lin

    PositA a = a ;

    UseComparA a = a ;

    ComparA a np = {
      s = \\g,n,c => a.s ! g ! n ! c ++ "కంటే" ++ np.s ! NPC Obl
      } ;

-- $SuperlA$ belongs to determiner syntax in $Noun$.

    ComplA2 a np = {
      s = \\g,n,c => a.s ! g ! n ! c ++ a.c2 ++ np.s ! NPC Obl
      } ;

    ReflA2 a = {
      s = a.s
      } ;

    SentAP ap sc = {
      s = \\g,n,c => ap.s ! g ! n ! c ++ sc.s
      } ;

    AdAP ada ap = {
      s = \\g,n,c => ada.s ++ ap.s ! g ! n ! c
      } ;

    AdvAP ap adv = {
      s = \\g,n,c => ap.s ! g ! n ! c ++ adv.s
      } ;

    UseA2 a = a ;

}
