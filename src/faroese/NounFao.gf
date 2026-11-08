concrete NounFao of Noun = CatFao ** open ResFao, Prelude in {
lin
  UseN n = {s = n.s ; p = n.s ! Indef ; g = n.g} ;
  UseN2 n = {s = n.s ; p = n.s ! Indef ; g = n.g} ;
  Use2N3 n = n ** {c2 = n.c2} ;
  Use3N3 n = n ** {c2 = n.c3} ;
  UsePN pn = mkNP pn.s Masc Sg P3 ;
  UsePron p = p ;
  RelNP np rs =
    np ** {s = \\c => np.s ! c ++ "," ++ rs.s ! np.g ! persNum np.n np.p} ;
  DetNP det = {
    s = \\c => det.s ! Masc ! c ;
    g = Masc ;
    n = det.n ;
    p = P3
  } ;
  PredetNP pred np = np ** {
    s = \\c => pred.s ++ np.s ! c
  } ;
  PPartNP np v2 = np ** {
    s = \\c => np.s ! c ++ v2.Participle ! Past
  } ;
  AdvNP np adv = np ** {
    s = \\c => np.s ! c ++ adv.s
  } ;
  ExtAdvNP np adv = np ** {
    s = \\c => np.s ! c ++ "," ++ adv.s
  } ;
  DetCN det cn = {
    s = \\c => det.s ! cn.g ! c ++ case det.d of {
      Weak   => cn.p ! det.n ! c ;
      Strong => cn.s ! det.sp ! det.n ! c
    } ;
    g = cn.g ;
    n = det.n ;
    p = P3
  } ;
  DefArt = {
    s = \\_,_,_,_ => [] ;
    sp = Def ;
    d = Strong
  } ;
  IndefArt = {
    s = \\b =>
      table {
        Masc => table {
                  Sg => case b of {
                          False => table {Nom => "ein" ; Acc => "ein" ; Dat => "einum" ; Gen => "eins"} ;
                          True  => \\_ => []
                        } ;
                  Pl => \\_ => []
                } ;
        Fem  => table {
                  Sg => case b of {
                          False => table {Nom => "ein" ; Acc => "eina" ; Dat => "einari" ; Gen => "einar"} ;
                          True  => \\_ => []
                        } ;
                  Pl => \\_ => []
                } ;
        Neuter => table {
                  Sg => case b of {
                          False => table {Nom => "eitt" ; Acc => "eitt" ; Dat => "einum" ; Gen => "eins"} ;
                          True  => \\_ => []
                        } ;
                  Pl => \\_ => []
                }
      } ;
    sp = Indef ;
    d = Strong
  } ;
  DetQuant quant num = {
    s = \\g,c => quant.s ! num.hasCard ! g ! num.n ! c ++
                 num.s ! g ! c ;
    n = num.n ;
    sp = quant.sp ;
    d = quant.d
  } ;
  DetQuantOrd quant num ord = {
    s = \\g,c => quant.s ! num.hasCard ! g ! num.n ! c ++
                 num.s ! g ! c ++ case quant.sp of {
                   Indef => ord.s ! Strong ! g ! num.n ! c ;
                   Def => ord.s ! Weak ! g ! num.n ! c
                 } ;
    n = num.n ;
    sp = quant.sp ;
    d = quant.d
  } ;
  NumSg = {
    s = \\_,_ => [] ;
    n = Sg ;
    hasCard = False
  } ;
  NumPl = {
    s = \\_,_ => [] ;
    n = Pl ;
    hasCard = False
  } ;
  NumCard card = card ** {hasCard = True} ;
  NumDigits digits = {s = \\_,_ => digits.s ; n = Pl} ;
  NumDecimal dec = {s = \\_,_ => dec.s ; n = Pl} ;
  NumNumeral numeral = {s=numeral.s ! NCard; n=numeral.n} ;
  AdNum adn card = {s = \\g,c => adn.s ++ card.s ! g ! c ; n = card.n} ;
  OrdDigits digits = {s = \\_,_,_,_ => digits.s ++ BIND ++ "."} ;
  OrdNumeral numeral = {s = \\_,g,n,c => numeral.s ! NOrd n ! g ! c} ;
  OrdSuperl a = {s = a.s} ;
  OrdNumeralSuperl numeral a = {
    s = \\af,g,n,c => numeral.s ! NOrd n ! g ! c ++ a.s ! af ! g ! n ! c
  } ;
  MassNP cn = {
    s = \\c => cn.s ! Indef ! Sg ! c ;
    g = cn.g ;
    n = Sg ;
    p = P3
  } ;
  PossPron pron = {
    s = \\_,g,n,c => pron.poss ! g ! n ! c ;
    -- A preposed possessive replaces, rather than licenses, the suffixed
    -- definite article: mín vinur, not *mín vinurin.
    sp = Indef ;
    d = Weak
  } ;
  ComplN2 n2 np = {
    s = \\sp,n,c => n2.s ! sp ! n ! c ++ n2.c2.s ++ np.s ! n2.c2.c ;
    p = \\n,c => n2.s ! Indef ! n ! c ++ n2.c2.s ++ np.s ! n2.c2.c ;
    g = n2.g
  } ;
  ComplN3 n3 np = n3 ** {
    s = \\sp,n,c => n3.s ! sp ! n ! c ++ n3.c2.s ++ np.s ! n3.c2.c ;
    p = \\n,c => n3.s ! Indef ! n ! c ++ n3.c2.s ++ np.s ! n3.c2.c ;
    c2 = n3.c3
  } ;
  AdjCN ap cn = {
    s = \\sp,n,c =>
      case sp of {
        Indef => ap.s ! Strong ! cn.g ! n ! c ++ cn.s ! sp ! n ! c ;
        Def => weakAdj ap cn.g n c ++ cn.s ! sp ! n ! c
      } ;
    p = \\n,c => ap.s ! Weak ! cn.g ! n ! c ++ cn.p ! n ! c ;
    g = cn.g
  } ;
  RelCN cn rs = {
    s = \\sp,n,c => cn.s ! sp ! n ! c ++ rs.s ! cn.g ! persNum n P3 ;
    p = \\n,c => cn.p ! n ! c ++ rs.s ! cn.g ! persNum n P3 ;
    g = cn.g
  } ;
  AdvCN cn adv = {
    s = \\sp,n,c => cn.s ! sp ! n ! c ++ adv.s ;
    p = \\n,c => cn.p ! n ! c ++ adv.s ;
    g = cn.g
  } ;
  SentCN cn sc = {
    s = \\sp,n,c => cn.s ! sp ! n ! c ++ sc.s ;
    p = \\n,c => cn.p ! n ! c ++ sc.s ;
    g = cn.g
  } ;
  ApposCN cn np = {
    s = \\sp,n,c => cn.s ! sp ! n ! c ++ np.s ! Nom ;
    p = \\n,c => cn.p ! n ! c ++ np.s ! Nom ;
    g = cn.g
  } ;
  PossNP cn np = {
    s = \\sp,n,c => cn.s ! sp ! n ! c ++ np.s ! Gen ;
    p = \\n,c => cn.p ! n ! c ++ np.s ! Gen ;
    g = cn.g
  } ;
  PartNP cn np = {
    s = \\sp,n,c => cn.s ! sp ! n ! c ++ "av" ++ np.s ! Dat ;
    p = \\n,c => cn.p ! n ! c ++ "av" ++ np.s ! Dat ;
    g = cn.g
  } ;
  CountNP det np = {
    s = \\c => det.s ! np.g ! c ++ "av" ++ np.s ! Dat ;
    g = np.g ;
    n = det.n ;
    p = P3
  } ;
  AdjDAP dap ap = dap ** {
    s = \\g,c => dap.s ! g ! c ++ ap.s ! dap.d ! g ! dap.n ! c
  } ;
  DetDAP det = det ;
  QuantityNP dec mu = {
    s = \\_ => dec.s ++ mu.s ;
    g = Neuter ;
    n = Pl ;
    p = P3
  } ;
}
