concrete AdjectivePes of Adjective = CatPes ** open ResPes, Prelude in {

  flags coding = utf8;
  lin

  PositA a = a ;
 	UseComparA a = a ;

  ComparA a np = a ** {
    s = \\m => a.s ! m ++ "تر" ++ "از" ++ np.s ! Bare ;
    adv = a.adv ++ "تر" ++ "از" ++ np.s ! Bare  ;
    } ;

---- $SuperlA$ belongs to determiner syntax in $Noun$.

  ComplA2 a np = a ** {
    s = \\m => np.s ! Bare ++ a.c2 ++ a.s ! m ;
    adv = np.s ! Bare ++ a.c2 ++ a.adv
    } ;

  ReflA2 a = a ** {
    s = \\m => a.s ! m ++ reflPron ! defaultAgr ; ---- need to be fixed
    adv = a.adv ++ reflPron ! defaultAgr
    } ;

  SentAP ap sc = ap ** {
    s = \\m => ap.s ! m ++ sc.s ;
    adv = ap.adv ++ sc.s
    } ;

  AdAP ada ap = ap ** {
    s = \\m => ada.s ++ ap.s ! m ;
    adv = ada.s ++ ap.adv ;
    } ;

  UseA2 a = a ;

  CAdvAP cadv ap np = ap ** {
    s = \\m => cadv.s ++ np.s ! Bare ++ ap.s ! m ;
    adv = cadv.s ++ ap.adv
    } ;

  AdjOrd ord = {
    s = \\_ => ord.s ;
    adv = ord.s ;
    isPre = ord.isPre
    } ;

  AdvAP ap adv = ap ** {
    s = \\m => ap.s ! m ++ adv.s ;
    adv = ap.adv ++ adv.s
    } ;
}
