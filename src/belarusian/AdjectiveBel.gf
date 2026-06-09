concrete AdjectiveBel of Adjective = CatBel ** open ResBel in {
lin
  PositA a = a ;
  ComparA a np = {s = \\c,gn => "больш" ++ a.s ! c ! gn ++ "за" ++ np.s ! Acc} ;
  ComplA2 a np = {s = \\c,gn => a.s ! c ! gn ++ prepNP a.c2 np} ;
  ReflA2 a = {s = \\c,gn => a.s ! c ! gn ++ "да сябе"} ;
  UseA2 a = a ;
  UseComparA a = {s = \\c,gn => "больш" ++ a.s ! c ! gn} ;
  CAdvAP cadv ap np = {s = \\c,gn => cadv.s ++ ap.s ! c ! gn ++ cadv.p ++ np.s ! Nom} ;
  AdjOrd ord = ord ;
  SentAP ap sc = {s = \\c,gn => ap.s ! c ! gn ++ sc.s} ;
  AdAP ada ap = {s = \\c,gn => ada.s ++ ap.s ! c ! gn} ;
  AdvAP ap adv = {s = \\c,gn => ap.s ! c ! gn ++ adv.s} ;
}
