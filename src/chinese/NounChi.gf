concrete NounChi of Noun = CatChi ** open ResChi, Prelude in {

  lin
    DetCN det cn = cn ** {
         det = case det.detType of {
            DTFull Sg => det.s ++ cn.c  ;  -- this house
            DTFull Pl => det.s ++ xie_s ;  -- these houses
            DTNum     => det.s ++ cn.c  ;  -- (these) five houses
            DTPoss    => det.s             -- our (five) houses
      }
    } ;

    UsePN pn = pn ** {det = []} ;
    UsePron p = p ** {det = []} ;

    DetNP det = {
      s = [] ;
      det = case det.detType of {
                   DTFull Pl => det.s ++ xie_s ;
                   DTPoss    => det.s ;
                   _         => det.s ++ ge_s
                   } ;
      } ; ----

    PredetNP pred np = np ** {s = pred.s ++ np.s} ; ---- possessive_s ++ np.s) ;

    PPartNP np v2 = np ** {s = (predV v2 v2.part).verb.s ++ possessive_s ++ np.s} ; ---- ??

    AdvNP np adv = np ** {s = adv.s ++ possessiveIf adv.hasDe ++ np.s} ;
    ExtAdvNP np adv = np ** {s = adv.s ++ possessiveIf adv.hasDe ++ embedInCommas np.s} ; ---- commas?

    DetQuant quant num = {
      s = case num.numType of {
        NTFull    => quant.pl ++ num.s ;  -- to avoid yi in indef
        NTVoid Pl => quant.pl ++ num.s ;
        _         => quant.s ++ num.s
        } ;
      detType = case num.numType of {
        NTFull => DTNum ;                     -- five
        NTVoid n => case quant.detType of {
          DTPoss => DTPoss ;                  -- our
          _ => DTFull n                       -- these/this
          }
       }
    } ;

    DetQuantOrd quant num ord = {
      s = quant.s ++ num.s ++ ord.s ;
      detType = case num.numType of {
        NTFull => DTNum ;                     -- five
        NTVoid n => DTFull n                  -- these/this ; also our, when ord is present
        }
      } ;

    PossPron p = {
      s,pl = p.s ++ possessive_s ;
      detType = DTPoss
      } ;

    NumSg = {s = [] ; numType = NTVoid Sg} ;
    NumPl = {s = [] ; numType = NTVoid Pl} ;

    NumCard n = n ** {numType = NTFull} ;
    NumDigits d = d ** {numType = NTFull} ;
    NumDecimal d = d ** {numType = NTFull} ;
    OrdDigits d = {s = ordinal_s ++ d.s} ;

    NumNumeral numeral = {s = numeral.p} ; -- liang instead of yi
    OrdNumeral numeral = {s = ordinal_s ++ numeral.s} ;

    AdNum adn num = {s = adn.s ++ num.s ; hasC = True} ;

    OrdSuperl a = {s = superlative_s ++ a.s ! Attr} ;
    OrdNumeralSuperl n a = {s = ordinal_s ++ n.s ++ superlative_s ++ a.s ! Attr} ; ---- to check AR 24/8/2014

    DefArt = mkQuant [] [] DTPoss ;          -- use that_Quant if you want the_s
    IndefArt = mkQuant yi_s [] DTNum ; -- (DTFull Sg) ;    -- empty in the plural

    MassNP cn = mkNP cn.s ;

    UseN n = n ;
    UseN2 n = n ;
    Use2N3 f = {s = f.s ; c = f.c ; c2 = f.c2} ;
    Use3N3 f = {s = f.s ; c = f.c ; c2 = f.c3} ;

    ComplN2 f x = {s = appPrep f.c2 (linNP x) ++ f.s ; c = f.c} ;
    ComplN3 f x = {s = appPrep f.c2 (linNP x) ++ f.s ; c = f.c ; c2 = f.c3} ;

    AdjCN ap cn = case ap.monoSyl of {
            True => {s = ap.s ! Attr ++ cn.s ; c = cn.c} ;
            False => {s = ap.s ! Attr ++ possessive_s ++ cn.s ; c = cn.c}
            } ;

    RelCN cn rs = {s = rs.s ++ cn.s ; c = cn.c} ;
    AdvCN cn ad = {s = ad.s ++ possessiveIf ad.hasDe ++ cn.s ; c = cn.c} ;
    SentCN cn cs = {s = cs.s ++ cn.s ; c = cn.c} ;
    ApposCN cn np = {s = linNP np ++ cn.s ; c = cn.c} ;

    RelNP np rs = np ** {s = rs.s ++ np.s} ;

    PossNP cn np = {s = linNP np ++ possessive_s ++ cn.s ; c = cn.c} ;
    PartNP cn np = {s = linNP np ++ possessive_s ++ cn.s ; c = cn.c} ;

    CountNP det np = np ** {det = det.s ++ ge_s ++ possessive_s ++ np.det} ; --- classifier from NP?

}
