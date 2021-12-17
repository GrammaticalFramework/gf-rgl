concrete AdjectiveChi of Adjective = CatChi ** open ResChi, Prelude in {

  lin

    PositA  a = a ** {hasAdA = False} ;

    --ComparA a np = complexAP (a.s ++ than_s ++ np.s) ;
   --  ComparA a np = complexAP (than_s ++ np.s ++ a.s) ;
   ComparA a np = a ** {
      s = table { adjPlace => than_s ++ np.s ++ a.s!adjPlace}
   };

    --UseComparA a = complexAP (geng_s ++ a.s) ;
   UseComparA a = geng ** {
      s = table { adjPlace => geng.s!adjPlace ++ a.s}
   };

   --  AdjOrd ord = complexAP ord.s ;
    AdjOrd ord = ord ** {
       s = table {adjPlace => ord.s!adjPlace}
    };

    -- CAdvAP ad ap np = complexAP (ap.s ++ ad.s ++ ad.p ++ np.s) ;
   --  CAdvAP ad ap np = complexAP (ad.s++ np.s++ad.p++ap.s ) ; --modified by ChenPeng 11.24
   CAdvAP ad ap np = ap ** {
      s = table {adjPlace => ad.s ++ np.s ++ ad.p ++ ap.s!adjPlace}
   };

   --  ComplA2 a np = complexAP (appPrep a.c2 np.s ++ a.s) ;
    ComplA2 a np = a ** {
       s= table { adjPlace => appPrep a.c2 np.s ++ a.s!adjPlace}
    };

   --  ReflA2 a = complexAP (a.s ++ appPrep a.c2 reflPron) ;
    ReflA2 a =  a ** {
       s = table {adjPlace => a.s!adjPlace ++ appPrep a.c2 reflPron}
    };


    -- SentAP ap sc = complexAP (ap.s ++ sc.s) ;
    SentAP ap sc =  ap ** {

        s = table { adjPlace => ap.s ! adjPlace ++ sc.s }
        } ;

   -- AdAP ada ap = {s = ada.s ++ ap.s ; monoSyl = False ; hasAdA = True} ;
   AdAP ada ap = ap ** {
      s = table { adjPlace => ada.s ++ ap.s ! adjPlace ; monoSyl = False; hasAdA = True}
   };

   UseA2 a = a ** {hasAdA = False} ;

   --  AdvAP ap adv = complexAP (adv.s ++ ap.s) ;
   AdvAP ap adv = ap ** {
      s = table { adjPlace => adv.s ++ ap.s ! adjPlace }
   };

}
