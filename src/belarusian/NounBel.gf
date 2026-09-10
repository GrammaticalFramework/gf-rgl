concrete NounBel of Noun = CatBel ** open ResBel, Prelude in {
lin
  UseN n = n ;

  DetCN det cn = {
    s = \\c => det.s ! c ! cn.g ++ cn.s ! c ! det.n ;
    a = {g=cn.g; n=det.n; p=P3}
  } ;
  UsePN pn = {
    s = pn.s ;
    a = {g=pn.g; n=pn.n; p=P3}
  } ;
  UsePron p = p ;

  PredetNP pred np = {
    s = \\c => pred.s ! c ! np.a.g ! np.a.n ++ np.s ! c ;
    a = np.a
  } ;
  PPartNP np v = {
    s = \\c => np.s ! c ++ v.participle ! np.a.g ! np.a.n ;
    a = np.a
  } ;
  AdvNP np adv = {
    s = \\c => np.s ! c ++ adv.s ;
    a = np.a
  } ;
  ExtAdvNP np adv = {
    s = \\c => np.s ! c ++ adv.s ;
    a = np.a
  } ;
  RelNP np rs = {
    s = \\c => np.s ! c ++ rs.s ;
    a = np.a
  } ;
  DetNP det = {
    s = \\c => det.s ! c ! Masc ;
    a = {g=Masc; n=det.n; p=P3}
  } ;

  DetQuant q n = {
    s = \\c,g => q.s ! c ! g ! n.n ++ n.s ! c ! g ;
    n = n.n
  } ;
  DetQuantOrd q n o = {
    s = \\c,g => q.s ! c ! g ! n.n ++ n.s ! c ! g ++ o.s ! c ! genNum g n.n ;
    n = n.n
  } ;
  NumSg = {s = \\_,_ => []; n = Sg} ;
  NumPl = {s = \\_,_ => []; n = Pl} ;
  NumCard card = {s = \\_,_ => card.s; n = card.n} ;
  NumDigits digits = {s = digits.s; n = Pl} ;
  NumDecimal dec = {s = dec.s; n = Pl} ;
  NumNumeral numeral = {s = numeral.s; n = Pl} ;
  AdNum adn card = {s = adn.s ++ card.s; n = card.n} ;

  OrdDigits d = adjFromStr d.s ;
  OrdNumeral n = adjFromStr n.s ;
  OrdSuperl a = {s = \\c,gn => "най" ++ a.s ! c ! gn} ;
  OrdNumeralSuperl n a = {s = \\c,gn => n.s ++ a.s ! c ! gn} ;

  IndefArt = {s = \\_,_,_ => []} ;
  DefArt = {s = \\_,_,_ => []} ;
  MassNP cn = {
    s = \\c => cn.s ! c ! Sg ;
    a = {g=cn.g; n=Sg; p=P3}
  } ;
  PossPron p = {s = \\_,_,_ => p.s ! Gen} ;

  ComplN2 n np = {
    s = \\c,num => n.s ! c ! num ++ prepNP n.c2 np ;
    voc = n.voc ;
    g = n.g
  } ;
  ComplN3 n np = n ** {c2 = n.c3} ;
  UseN2 n = n ;
  Use2N3 n = n ** {c2 = n.c2} ;
  Use3N3 n = n ** {c2 = n.c3} ;

  AdjCN ap cn = {
    s = \\c,num => ap.s ! c ! genNum cn.g num ++ cn.s ! c ! num ;
    voc = cn.voc ;
    g = cn.g
  } ;
  RelCN cn rs = {
    s = \\c,num => cn.s ! c ! num ++ rs.s ;
    voc = cn.voc ;
    g = cn.g
  } ;
  AdvCN cn adv = {
    s = \\c,num => cn.s ! c ! num ++ adv.s ;
    voc = cn.voc ;
    g = cn.g
  } ;
  SentCN cn sc = {
    s = \\c,num => cn.s ! c ! num ++ sc.s ;
    voc = cn.voc ;
    g = cn.g
  } ;
  ApposCN cn np = {
    s = \\c,num => cn.s ! c ! num ++ np.s ! Nom ;
    voc = cn.voc ;
    g = cn.g
  } ;
  PossNP cn np = {
    s = \\c,num => cn.s ! c ! num ++ np.s ! Gen ;
    voc = cn.voc ;
    g = cn.g
  } ;
  PartNP cn np = {
    s = \\c,num => cn.s ! c ! num ++ np.s ! Gen ;
    voc = cn.voc ;
    g = cn.g
  } ;
  CountNP det np = {
    s = \\c => det.s ! c ! Masc ++ "з" ++ np.s ! Gen ;
    a = {g=Masc; n=det.n; p=P3}
  } ;

  AdjDAP dap ap = {
    s = \\c,g => dap.s ! c ! g ++ ap.s ! c ! genNum g dap.n ;
    n = dap.n
  } ;
  DetDAP det = det ;

  QuantityNP dec mu = mkSimpleNP (dec.s ++ mu.s) Neuter Sg P3 ;
}
