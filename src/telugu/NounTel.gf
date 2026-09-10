concrete NounTel of Noun = CatTel ** open ResTel, Prelude in {

  flags optimize=all_subs ;

  lin
    DetCN det cn = {
      s = \\c => det.s ! cn.g ! npcase2case c ++ toNP (cn.s ! det.n) c ;
      a = agrP3 cn.g det.n
      } ;

    UsePN pn = {s = \\c => toNP pn.s c ; a = agrP3 pn.g Sg} ;
    UsePron p = {s = \\c => p.s ! np2pronCase c ; a = p.a} ;
--
    PredetNP pred np = {
      s = \\c => pred.s ++ np.s ! c ;
      a = np.a
      } ;
--
    PPartNP np v2 = {
      s = \\c => np.s ! c ++ v2.s ! VStem ;
      a = np.a
      } ;
--
--    RelNP np rs = {
--      s = \\c => np.s ! c ++ "," ++ rs.s ! np.a ;
--      a = np.a
--      } ;
--
    AdvNP np adv = {
      s = \\c => np.s ! c ++ adv.s ;
      a = np.a
      } ;
--
    DetQuantOrd quant num ord = {
      s = \\g,c => quant.s ! num.n ! g ! c ++ num.s ++ ord.s ;
      n = num.n
      } ;

    DetQuant quant num = {
      s = \\g,c => quant.s ! num.n ! g ! c ++ num.s ;
      n = num.n
      } ;

--    DetNP det = {
--      s = \\c => det.s ; ---- case
--      a = agrP3 det.n
--      } ;
--
    PossPron p = {s = \\_,_,_ => p.s ! PPoss} ;

    NumSg = {s = []; n = Sg} ;
    NumPl = {s = []; n = Pl} ;

    OrdSuperl a = {s = "అత్యంత" ++ a.s ! Masc ! Sg ! Dir} ;

    NumCard n = {s = n.s ; n = Pl} ;
    NumDecimal n = {s = n.s ; n = n.n} ;
    AdNum adn card = {s = adn.s ++ card.s ; n = card.n} ;
    OrdNumeral numeral = {s = numeral.s} ;
--
--    NumDigits n = {s = n.s ! NCard ; n = n.n} ;
--    OrdDigits n = {s = n.s ! NOrd} ;
--
    NumNumeral numeral = {s = numeral.s ; n = Pl} ;
--    OrdNumeral numeral = {s = numeral.s ! NOrd} ;
--
--    AdNum adn num = {s = adn.s ++ num.s ; n = num.n} ;
--
--    OrdSuperl a = {s = a.s ! AAdj Superl} ;
--
    DetArtOrd art num ord = {
      s = art.s ++ num.s ++ ord.s ;
      n = num.n
      } ;
--
    DetArtCard art card = {s = art.s ++ card.s ; n = card.n} ;

    DetDAP det = {s = det.s ; n = det.n} ;
    AdjDAP dap ap = {
      s = \\g,c => ap.s ! g ! dap.n ! c ++ dap.s ! g ! c ;
      n = dap.n
      } ;

      DefArt = {s = \\_,_,_ => []} ;
      IndefArt = {s = \\_,_,_ => []} ;

    MassNP cn = {s = \\c => cn.s ! Sg ! npcase2case c ; a = agrP3 cn.g Sg} ;

      UseN n = n ;
    UseN2 n = n ;
--
--    Use2N3 f = {
--      s = \\n,c => f.s ! n ! Nom ;
--      g = f.g ;
--      c2 = f.c2
--      } ;
--
--    Use3N3 f = {
--      s = \\n,c => f.s ! n ! Nom ;
--      g = f.g ;
--      c2 = f.c3
--      } ;
--
    ComplN2 f x = {s = \\n,c => f.s ! n ! c ++ f.c2 ++ x.s ! NPC c ; g = f.g} ;
--    ComplN3 f x = {
--      s = \\n,c => f.s ! n ! Nom ++ f.c2 ++ x.s ! c ;
--      g = f.g ;
--      c2 = f.c3
--      } ;

    AdjCN ap cn = {
      s = \\n,c => ap.s ! cn.g ! n ! c ++ cn.s ! n ! c ;
      g = cn.g
      } ;

    RelCN cn rs = {
      s = \\n,c => rs.s ++ cn.s ! n ! c ;
      g = cn.g
      } ;

--    RelCN cn rs = {
--      s = \\n,c => cn.s ! n ! c ++ rs.s ! agrgP3 n cn.g ;
--      g = cn.g
--      } ;
    AdvCN cn ad = {s = \\n,c => cn.s ! n ! c ++ ad.s ; g = cn.g} ;
--
    SentCN cn sc = {s = \\n,c => cn.s ! n ! c ++ sc.s ; g = cn.g} ;

    PossNP cn np = {
      s = \\n,c => np.s ! NPC Obl ++ cn.s ! n ! c ;
      g = cn.g
      } ;

    PartNP cn np = {
      s = \\n,c => np.s ! NPC Obl ++ cn.s ! n ! c ;
      g = cn.g
      } ;

    ApposCN cn np = {
      s = \\n,c => cn.s ! n ! c ++ np.s ! NPC Dir ;
      g = cn.g
      } ;

    RelNP np rs = {
      s = \\c => rs.s ++ np.s ! c ;
      a = np.a
      } ;
--
--    ApposCN cn np = {s = \\n,c => cn.s ! n ! Nom ++ np.s ! c ; g = cn.g} ;
--
}
