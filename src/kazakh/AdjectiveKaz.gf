concrete AdjectiveKaz of Adjective = CatKaz ** open ResKaz in {
  lin
    PositA a = a ;
    ComparA a np = {s=np.s ! Ablat ++ a.s} ;
    UseComparA a = {s=a.s} ;
    ComplA2 a np = {s=complNP a.c2 np ++ a.s} ;
    ReflA2 a = {s=a.s} ;
    UseA2 a = {s=a.s} ;
    SentAP ap sc = {s=sc.s ++ ap.s} ;
    AdAP ada ap = {s=ada.s ++ ap.s} ;
    AdvAP ap adv = {s=adv.s ++ ap.s} ;
    CAdvAP cadv ap np = {s=cadv.s ++ ap.s ++ cadv.p ++ np.s ! Ablat} ;
}
