concrete AdjectiveBel of Adjective = CatBel ** open ResBel in {
lin
  PositA a = a ;
  ComparA a np = {s = \\c,gn => "больш" ++ a.s ! c ! gn ++ "за" ++ np.s ! Acc; adv = "больш" ++ a.adv ++ "за" ++ np.s ! Acc; post = a.post} ;
  ComplA2 a np = {s = \\c,gn => a.s ! c ! gn ++ prepNP a.c2 np; adv = a.adv ++ prepNP a.c2 np; post = a.post} ;
  ReflA2 a = {s = \\c,gn => a.s ! c ! gn ++ "да сябе"; adv = a.adv ++ "да сябе"; post = a.post} ;
  UseA2 a = a ;
  UseComparA a = {s = \\c,gn => "больш" ++ a.s ! c ! gn; adv = "больш" ++ a.adv; post = a.post} ;
  CAdvAP cadv ap np = {s = \\c,gn => cadv.s ++ ap.s ! c ! gn ++ cadv.p ++ np.s ! Nom; adv = cadv.s ++ ap.adv ++ cadv.p ++ np.s ! Nom; post = ap.post} ;
  AdjOrd ord = ord ;
  SentAP ap sc = {s = \\c,gn => ap.s ! c ! gn ++ sc.s; adv = ap.adv ++ sc.s; post = ap.post} ;
  AdAP ada ap = {s = \\c,gn => ada.s ++ ap.s ! c ! gn; adv = ada.s ++ ap.adv; post = ap.post} ;
  AdvAP ap adv = {s = \\c,gn => ap.s ! c ! gn ++ adv.s; adv = ap.adv ++ adv.s; post = ap.post} ;
}
