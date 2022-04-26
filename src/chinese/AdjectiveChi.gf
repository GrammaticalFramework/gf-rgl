concrete AdjectiveChi of Adjective = CatChi ** open ResChi, Prelude in {

  lin

    PositA  a = a ** {hasAdA = False} ;

    ComparA a np = a ** {
      s = table {_=> than_s ++ np.s ++ a.s!Attr};
      hasAdA = False

    };

    UseComparA a = a ** {
      s = table { _=> geng_s ++ a.s!Attr};
      hasAdA = False
    };

    AdjOrd ord = ord ** {
     s = table {
      adjPlace => ord.s
      -- Attr => ord.s; --"first is he" ;
      -- Pred => ord.s --"he is first"
      } ;
     hasAdA = False;
     monoSyl = True -- to do and figure out in relation to Ord = {s : Str}
    };

    CAdvAP ad ap np = ap ** {
      s = table {adjPlace => ad.s ++ np.s ++ ad.p ++ ap.s!adjPlace}
    };

    ComplA2 a np = a ** {
       s= table { adjPlace => appPrep a.c2 np.s ++ a.s!adjPlace};
       hasAdA = False
     };


    ReflA2 a =  a ** {
       s = table {adjPlace => a.s!adjPlace ++ appPrep a.c2 reflPron};
       hasAdA = False
     };


    SentAP ap sc =  ap ** {
        s = table { adjPlace => ap.s ! adjPlace ++ sc.s }
    } ;

   -- AdAP ada ap = {s = ada.s ++ ap.s ; monoSyl = False ; hasAdA = True} ;
    AdAP ada ap = ap ** {
      s = table { adjPlace => ada.s ++ ap.s ! adjPlace };
      monoSyl = False;
      hasAdA = True
    };

    UseA2 a = a ** {hasAdA = False} ;

    AdvAP ap adv = ap ** {
      s = table { adjPlace => adv.s ++ ap.s ! adjPlace }
    };

}
