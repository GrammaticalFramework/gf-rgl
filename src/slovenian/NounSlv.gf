concrete NounSlv of Noun = CatSlv ** open ResSlv,Prelude in {

  lin
    DetCN det cn = {
      s = \\c => det.s ! agender2gender cn.g ! c ++ 
                 case det.n of {
                   UseNum n => cn.s ! det.spec ! c ! n ;
                   UseGen   => cn.s ! det.spec ! Gen ! Pl
                 } ;
      a = {g = agender2gender cn.g ;
           n = case det.n of {
                 UseNum n => n ;
                 UseGen   => Pl
               } ;
           p = P3
          } ;
      isPron = False
      } ;

    UsePN pn = {
      s = pn.s;
      a = {g=agender2gender pn.g; n=pn.n; p=P3};
      isPron = False
      } ;

    UsePron p = {
      s = p.s;
      a = p.a;
      isPron = True
    } ;

    AdvNP np adv = {
      s = \\c => np.s ! c ++ adv.s ;
      a = np.a ;
      isPron = False  -- KA: guessed
    } ;

    PredetNP pred np = np ** {
      s = \\c => pred.s ++ np.s ! c
    } ;

    ExtAdvNP np adv = AdvNP np adv ;

    RelNP np rs = np ** {s = \\c => np.s ! c ++ "," ++ rs.s ! np.a} ;

    DetQuant quant num = {
      s    = \\g,c => quant.s ! g ! c ! (numAgr2num ! num.n) ++ num.s ! g ! c;
      spec = quant.spec ;
      n    = num.n ;
      } ;

    DetQuantOrd quant num ord = {
      s = \\g,c => quant.s ! g ! c ! (numAgr2num ! num.n) ++ num.s ! g ! c ++
                     ord.s ! g ! c ! (numAgr2num ! num.n) ;
      spec = quant.spec ; n = num.n
    } ;

    DetNP det = {
      s = det.s ! Masc ;
      a = {g=Masc; n=case det.n of {UseNum n=>n; UseGen=>Pl}; p=P3} ;
      isPron = False
      } ;

    PossPron p = {
      s    = p.poss ;
      spec = Indef
      } ;

    NumSg = {s = \\_,_ => []; n = UseNum Sg} ;
    NumDl = {s = \\_,_ => []; n = UseNum Dl} ; --not working?
    NumPl = {s = \\_,_ => []; n = UseNum Pl} ;

    NumCard n = n ;

    NumNumeral numeral = numeral ;
    NumDigits digits = {
      s = \\g,c => digits.s;
      n = digits.n
      } ;
    NumDecimal decimal = {
      s = \\g,c => decimal.s;
      n = decimal.n
      } ;

    AdNum adn card = card ** {s = \\g,c => adn.s ++ card.s ! g ! c} ;

    OrdDigits digits = {s = \\_,_,_ => digits.s ++ BIND ++ "."} ;
    OrdNumeral numeral = {s = \\g,c,_ => numeral.s ! g ! c} ;
    OrdSuperl a = {s = \\g,c,n => "naj" ++ BIND ++ a.s ! APosit g n c} ;
    OrdNumeralSuperl numeral a = {
      s = \\g,c,n => numeral.s ! g ! c ++ "naj" ++ BIND ++ a.s ! APosit g n c
    } ;

    DefArt = {
      s    = \\_,_,_ => "" ;
      spec = Def
      } ;

    IndefArt = {
      s    = \\_,_,_ => "" ;
      spec = Indef
      } ;

    MassNP n = {
      s = \\c => n.s ! Indef ! c ! Sg ;
      a = {g=agender2gender n.g; n=Sg; p=P3} ;
      isPron = False
      } ;

    UseN n = {s = \\_ => n.s; g = n.g} ;

    AdjCN ap cn = {
      s = \\spec,c,n => ap.s ! spec ! cn.g ! c ! n ++ cn.s ! Indef ! c ! n ;
      g = cn.g
      } ;
    AdvCN cn ad = {s = \\spec,c,n => cn.s ! spec ! c ! n ++ ad.s ; g = cn.g} ;

    ComplN2 n np = {
      s = \\_,c,num => n.s ! c ! num ++ n.c.s ++ np.s ! n.c.c ; g = n.g
    } ;
    ComplN3 n np = n ** {s = \\c,num => n.s ! c ! num ++ n.c.s ++ np.s ! n.c.c} ;
    UseN2 n = {s = \\_ => n.s; g = n.g} ;
    Use2N3 n = n ;
    Use3N3 n = n ;

    RelCN cn rs = cn ** {
      s = \\sp,c,n => cn.s ! sp ! c ! n ++ rs.s ! {g=agender2gender cn.g;n=n;p=P3}
    } ;
    SentCN cn sc = cn ** {s = \\sp,c,n => cn.s ! sp ! c ! n ++ sc.s} ;
    ApposCN cn np = cn ** {s = \\sp,c,n => cn.s ! sp ! c ! n ++ np.s ! c} ;
    PossNP cn np = cn ** {s = \\sp,c,n => cn.s ! sp ! c ! n ++ np.s ! Gen} ;
    PartNP cn np = cn ** {s = \\sp,c,n => cn.s ! sp ! c ! n ++ "od" ++ np.s ! Gen} ;

    CountNP det np = {
      s = \\c => det.s ! np.a.g ! c ++ "od" ++ np.s ! Gen ;
      a = {g=np.a.g;n=numAgr2num ! det.n;p=P3}; isPron=False
    } ;

    DetDAP det = {
      s = \\_,g,c,_ => det.s ! agender2gender g ! c; n = det.n
    } ;
    AdjDAP dap ap = dap ** {
      s = \\sp,g,c,n => dap.s ! sp ! g ! c ! n ++ ap.s ! sp ! g ! c ! n
    } ;

    QuantityNP decimal mu = {
      s = \\_ => decimal.s ++ mu.s;
      a = {g=Fem;n=numAgr2num ! decimal.n;p=P3}; isPron=False
    } ;

}
