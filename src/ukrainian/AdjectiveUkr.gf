concrete AdjectiveUkr of Adjective = CatUkr ** open ResUkr in {
lin
  PositA a = a ;
  ComparA a np = {s=\\c,gn => "більш" ++ a.s ! c ! gn ++ "ніж" ++ np.s ! Nom} ;
  ComplA2 a np = a ** {
    s = \\c,gn => a.s ! c ! gn ++ prepNP a.c2 np
  } ;
  ReflA2 a = a ** {
    s = \\c,gn => a.s ! c ! gn ++ a.c2.s ++ "себе"
  } ;
  UseA2 a = a ;
  UseComparA a = {s=\\c,gn => "більш" ++ a.s ! c ! gn} ;
  CAdvAP cadv ap np = {s=\\c,gn => cadv.s ++ ap.s ! c ! gn ++ cadv.p ++ np.s ! Nom} ;
  AdjOrd ord = ord ;
  SentAP ap sc = ap ** {
    s = \\c,gn => ap.s ! c ! gn ++ sc.s
  } ;
  AdAP ada ap = ap ** {
    s = \\c,gn => ada.s ++ ap.s ! c ! gn
  } ;
  AdvAP ap adv = ap ** {
    s = \\c,gn => ap.s ! c ! gn ++ adv.s
  } ;
}
