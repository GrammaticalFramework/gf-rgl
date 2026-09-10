concrete NounUkr of Noun = CatUkr ** open ResUkr, Prelude in {
lin
  UseN n = n ;

  DetCN det cn = {
    s = \\c => det.s ! c ! cn.g ++ cn.s ! Nom ! Sg ;
    g = cn.g ;
    n = det.n ;
    p = P3
  } ;
  UsePN pn = {s = \\_ => pn.s ; g = Masc ; n = Sg ; p = P3} ;
  UsePron p = p ;

  PredetNP pred np = np ** {
    s = \\c => pred.s ! c ! np.g ! np.n ++ np.s ! c
  } ;
  AdvNP np adv = np ** {s = \\c => np.s ! c ++ adv.s} ;
  ExtAdvNP np adv = np ** {s = \\c => np.s ! c ++ "," ++ adv.s} ;
  DetNP det = {
    s = \\c => det.s ! c ! Masc ;
    g = Masc ;
    n = det.n ;
    p = P3
  } ;

  DetQuant q num = {
    s = \\c,g => q.s ! c ! g ! num.n ++ num.s ;
    n = num.n
  } ;
  DetQuantOrd q num ord = {
    s = \\c,g => q.s ! c ! g ! num.n ++ num.s ++ ord.s ! c ! genNum g num.n ;
    n = num.n
  } ;
  NumSg = {s = [] ; n = Sg} ;
  NumPl = {s = [] ; n = Pl} ;
  NumCard card = card ;
  NumDigits digits = {s = digits.s ; n = Pl} ;
  NumDecimal dec = {s = dec.s ; n = Pl} ;
  NumNumeral numeral = {s = numeral.s ; n = Pl} ;
  AdNum adn card = card ** {s = adn.s ++ card.s} ;
  OrdDigits digits = {s=\\_,_ => digits.s} ;
  OrdNumeral numeral = {s=\\_,_ => numeral.s} ;
  OrdSuperl a = {s=\\c,gn => "най" ++ a.s ! c ! gn} ;
  OrdNumeralSuperl numeral a = {s=\\c,gn => numeral.s ++ "най" ++ a.s ! c ! gn} ;

  IndefArt = {s = \\_,_,_ => []} ;
  DefArt = {s = \\_,_,_ => []} ;
  MassNP cn = {
    s = \\_ => cn.s ! Nom ! Sg ;
    g = cn.g ;
    n = Sg ;
    p = P3
  } ;
  PossPron p = {
    s = \\_,g,n => possPron p.p p.g p.n g n
  } ;

  ComplN2 n2 np = n2 ** {
    s = \\c,num => n2.s ! c ! num ++ prepNP n2.c2 np
  } ;
  ComplN3 n3 np = n3 ** {
    c2 = n3.c2 ;
    s = \\c,num => n3.s ! c ! num ++ prepNP n3.c3 np
  } ;
  UseN2 n2 = n2 ;
  Use2N3 n3 = n3 ** {c2 = n3.c2} ;
  Use3N3 n3 = n3 ** {c2 = n3.c3} ;

  AdjCN ap cn = cn ** {
    s = \\c,num => ap.s ! c ! genNum cn.g num ++ cn.s ! c ! num
  } ;
  RelCN cn rs = cn ** {
    s = \\c,n => cn.s ! c ! n ++ rs.s ! cn.g ! n
  } ;
  AdvCN cn adv = cn ** {
    s = \\c,n => cn.s ! c ! n ++ adv.s
  } ;
  SentCN cn sc = cn ** {
    s = \\c,n => cn.s ! c ! n ++ sc.s
  } ;
  ApposCN cn np = cn ** {
    s = \\c,n => cn.s ! c ! n ++ np.s ! Nom
  } ;
  PossNP cn np = cn ** {
    s = \\c,n => cn.s ! c ! n ++ np.s ! Gen
  } ;
  PartNP cn np = cn ** {
    s = \\c,n => cn.s ! c ! n ++ np.s ! Gen
  } ;
  CountNP det np = {
    s = \\c => det.s ! c ! np.g ++ "з" ++ np.s ! Gen ;
    g = np.g ;
    n = det.n ;
    p = P3
  } ;
  QuantityNP decimal mu = {
    s = \\_ => case mu.isPre of {
      True => mu.s ++ decimal.s ;
      False => decimal.s ++ mu.s
    } ;
    g = Masc ;
    n = Pl ;
    p = P3
  } ;

  AdjDAP dap ap = dap ** {
    s = \\c,g => dap.s ! c ! g ++ ap.s ! c ! genNum g dap.n
  } ;
  DetDAP det = det ;
}
