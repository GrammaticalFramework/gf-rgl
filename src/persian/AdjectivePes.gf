concrete AdjectivePes of Adjective = CatPes ** open ResPes, Prelude in {

  flags coding = utf8;
  lin

  PositA a = a ** {s = a.s ! Positive} ;
 	UseComparA a = a ** {s = a.s ! Comparative} ;

  ComparA a np = a ** {
    s = \\m => a.s ! Comparative ! m ++ "تر" ++ "از" ++ np2str np ;
    adv = a.adv ++ "تر" ++ "از" ++ np2str np ;
    } ;

---- $SuperlA$ belongs to determiner syntax in $Noun$.

  ComplA2 a np = a ** {
    s = \\m => a.c2 ++ np2str np ++ a.s ! Positive ! m ;
    adv = np2str np ++ a.c2 ++ a.adv
    } ;

  ReflA2 a = a ** {
    s = \\m => a.s ! Positive ! m ++ reflPron ! defaultAgr ! Bare ; ---- s needs to be an inflection table from Agr to string, to choose the right reflexive pronoun
    adv = a.adv ++ reflPron ! defaultAgr ! Bare
    } ;

  SentAP ap sc = ap ** {
    s = \\m => ap.s ! m ++ sc.s ;
    adv = ap.adv ++ sc.s
    } ;

  AdAP ada ap = ap ** {
    s = \\m => ada.s ++ ap.s ! m ;
    adv = ada.s ++ ap.adv ;
    } ;

  UseA2 = PositA ;

  CAdvAP cadv ap np = ap ** {
    s = \\m => cadv.s ++ np2str np ++ ap.s ! m ;
    adv = cadv.s ++ ap.adv
    } ;

  AdjOrd ord = {
    s = \\_ => ord.s ;
    adv = ord.s ;
    isPre = ord.isPre ;
    afterPrefix = False ;
    } ;

  AdvAP ap adv = ap ** {
    s = \\m => ap.s ! m ++ adv.s ;
    adv = ap.adv ++ adv.s
    } ;
}
