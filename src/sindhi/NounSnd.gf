concrete NounSnd of Noun = CatSnd ** open ResSnd, Prelude in {

  flags optimize=all_subs ;

  lin
    DetCN det cn = {
      s = \\c => detcn2NP det cn c det.n  ;
      a = agrP3 cn.g det.n ;
      isPron = False
      } ;

    UsePN pn = {s = \\c => toNP pn.s c ; a = agrP3 pn.g Sg ; isPron = False} ;
    UsePron p = {s = \\c => np2pronCase p.s c p.a ; a = p.a ; isPron = True} ;

    PredetNP pred np = {
      s = \\c => pred.s ++ np.s ! c ;
      a = np.a;
      isPron = np.isPron
      } ;

    PPartNP np v2 = {
      s = \\c => v2.s ! VF Perf (fromAgr np.a).p (fromAgr np.a).n (fromAgr np.a).g   ++ np.s ! c   ;
      a = np.a ;
      isPron = np.isPron
      } ;

    RelNP np rs = {
	  s = \\c => np.s ! c  ++ rs.s ! np.a ;
      a = np.a ;
      isPron = np.isPron
      } ;

    AdvNP np adv = {
      s = \\c => np.s ! c ++ adv.s ! (fromAgr np.a).g ;
      a = np.a ;
      isPron = np.isPron
      } ;

    DetQuantOrd quant num ord = {
--      s = \\ c => detquant2det quant.s num.s c  ++ ord.s ; 
        s = \\n,g => quant.s!n!g!Dir ++ num.s ++ ord.s;
      n = num.n
      } ;

    DetQuant quant num = {
--      s = \\c => detquant2det quant.s num.s c; 
        s = \\n,g => quant.s!n!g!Dir ++ num.s;
	  n = num.n
      } ;

    DetNP det = {
      s = \\c => det2NP det c ; ---- case
      a = agrP3 Masc Sg ;
      isPron = False
      } ;

    PossPron p = {s = \\_,_,_ => p.ps ; a = p.a} ;

    NumSg = {s = []; n = Sg} ;
    NumPl = {s = []; n = Pl} ;

    NumCard n = n ** {hasCard = True} ;

    NumDigits n = {s = n.s ! NCard ; n = n.n} ;
    OrdDigits n = {s = n.s ! NOrd; n = n.n} ;

    NumNumeral numeral = {s = numeral.s ! NCard; n = numeral.n} ;
    OrdNumeral numeral = {s = numeral.s ! NOrd ; n = numeral.n} ;

    AdNum adn num = {s = adn.s ++ num.s ; n = num.n} ;

    OrdSuperl a = {s = a.s ! Sg ! Masc ! Dir ; n = Sg} ;



    DetArtSg art cn = {
      s = \\c => art.s ++ toNP (cn.s ! Sg) c ;
      a = agrP3 cn.g Sg
      } ;

    DetArtPl art cn = {
      s = \\c => art.s ++ toNP (cn.s ! Pl) c ;
      a = agrP3 cn.g Pl
      } ;

    DefArt = {s = \\_,_,_ => [] ; a = defaultAgr} ;
    IndefArt = {s = \\_,_,_ => [] ; a =defaultAgr } ;

    MassNP cn = {s = \\c => toNP (cn.s ! Sg) c ; a = agrP3 cn.g Sg ; isPron = False} ;

    UseN n = n ;
    UseN2 n = { s = n.s  ; g = n.g };

    Use2N3 f = {
      s = f.s;
      g = f.g ;
      c2 = f.c2;
      c3 = f.c3
      } ;

    Use3N3 f = {
      s = f.s ;
      g = f.g ;
      c2 = f.c2;
      c3 = f.c3
      } ;

    ComplN2 f x = {s = \\n,c => case c of {
       Dir => x.s ! NPC c ++ f.c2  ++ f.s ! n ! c ;
	   Obl => x.s ! NPC c ++ f.c3 ++ f.s ! n ! c ;
	   Abl => x.s ! NPC c ++ f.c3 ++ f.s ! n ! c ;
	   ResSnd.Voc => x.s ! NPC c ++ f.c3 ++ f.s ! n ! c 
	   };
	   g = f.g;
	   };
    ComplN3 f x = {
      s = \\n,c =>  x.s ! NPObj ++ f.c4  ++ f.s ! n ! Dir  ;
      g = f.g ;
      c2 = f.c2;
      c3 = f.c3
      } ;

    AdjCN ap cn = {
      s = \\n,c => ap.s ! n ! cn.g ! c ++ cn.s ! n ! c ;
      g = cn.g
      } ;

    RelCN cn rs = {
      s = \\n,c => cn.s ! n ! c ++ rs.s ! agrP3 cn.g n ;
      g = cn.g
      } ;
    AdvCN cn ad = {s = \\n,c => cn.s ! n ! c ++ ad.s ! cn.g ; g = cn.g} ;

    SentCN cn sc = {s = \\n,c => cn.s ! n ! c ++ sc.s ; g = cn.g} ;

    ApposCN cn np = {s = \\n,c => cn.s ! n ! Dir ++ np.s ! NPC c ; g = cn.g} ;

}
