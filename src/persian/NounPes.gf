concrete NounPes of Noun = CatPes ** open ResPes, Prelude in {

  flags optimize=all_subs ;

  lin
    DetCN det cn = { s = \\_ =>
      case <det.isNum,det.fromPron> of {
        <False,True>  => cn.s ! aEzafa ! det.n ++ det.s ;
        <False,False> => det.s ++ cn.s ! bEzafa ! det.n ;
        <True,True>   => cn.s ! aEzafa ! Sg ++ det.s ; -- noun modified by a number is invariably singular
        <True,False>  => det.s ++ cn.s ! bEzafa ! Sg
     };
      a = agrPesP3 det.n ;
      animacy = cn.animacy
      } ;

    UsePN pn = {s = \\_ => pn.s ; a = agrPesP3 Sg ; animacy = pn.animacy } ;
    UsePron p = {s = \\_ => p.s ; a = p.a ; animacy = Animate} ;

    PredetNP pred np = np ** {
      s = \\ez => pred.s ++ np.s ! ez
      } ;

    PPartNP np v2 = np ** {
      s = \\ez => np.s ! ez ++ partNP (v2.s ! Root1)
      } ;

    RelNP np rs = np ** {
      s = \\ez => np.s ! ez ++ rs.s ! np.a
      } ;

    AdvNP np adv = np ** {
      s = \\ez => np.s ! NPC aEzafa ++ adv.s
      } ;

    DetQuantOrd quant num ord = {
      s = quant.s ! num.n ++ num.s ++ ord.s ;
      isNum = orB num.isNum ord.isNum ;
      fromPron = quant.fromPron ;
      n = num.n
      } ;

    DetQuant quant num = {
      s = quant.s ! num.n ++ num.s;
      isNum = num.isNum;
      fromPron = quant.fromPron ;
      n = num.n
      } ;

    DetNP det = {
      s = \\_ => det.s  ; ---- case
      a = agrPesP3 det.n ;
      animacy = Inanimate
      } ;

    PossPron p = {s = \\_ => p.ps ; a = p.a ; fromPron = True} ;

    NumSg = {s = [] ; n = Sg ; isNum = False} ;
    NumPl = {s = [] ; n = Pl ; isNum = False} ;
-- from here
    NumCard n = n ** {isNum = True} ;

    NumDigits n = n ** {s = n.s ! NCard; isNum = True} ;
    OrdDigits n = n ** {s = n.s ! NOrd ; isNum = True} ;

    NumNumeral n = n ** {s = n.s ! NCard; isNum = True} ;
    OrdNumeral n = n ** {s = n.s ! NOrd ; isNum = True} ;
-- to here
    AdNum adn num = num ** {s = adn.s ++ num.s} ;

    OrdSuperl a = {s = a.s ! bEzafa ++ taryn; n = Sg ; isNum=False} ; -- check the form of adjective

    DefArt = {s = \\_ => [] ; a = defaultAgrPes ; fromPron = False} ;
    IndefArt = {s = table { Sg => IndefArticle ; Pl => []} ; a =defaultAgrPes ; fromPron = False} ;

    MassNP cn = {s =\\c => case c of {
      NPC bEzafa => cn.s ! bEzafa ! Sg ;
      NPC aEzafa => cn.s ! aEzafa ! Sg ;
      NPC enClic => cn.s ! enClic ! Sg
      };
      a = agrPesP3 Sg ;
      animacy = cn.animacy
      } ;

    UseN n = n ;
    UseN2 n = n ;

    Use2N3 f = {
      s = f.s;
      c = f.c2;
      animacy = f.animacy;
      definitness = True
      } ;

   Use3N3 f = {
      s = f.s ;
      c = f.c3;
      animacy = f.animacy;
      definitness = True
      } ;

    ComplN2 f x = {
      s = \\ez,n => f.s ! ez ! n  ++ f.c ++ x.s ! NPC ez ;
      animacy = f.animacy;
      definitness = True
     };
    ComplN3 f x = {
      s = \\ez,n => f.s ! ez ! n ++ f.c2 ++ x.s ! NPC ez ;
      c = f.c3;
      animacy = f.animacy;
      definitness = True;
      } ;

    AdjCN ap cn = cn ** {
      s = \\ez,n =>  cn.s ! aEzafa ! n ++ ap.s ! ez -- check the form of adjective and also cn.s!ez!n changed from cn.s!aEzafa!n to have correct enclicitic form other wise it creats wrong enclictic form of old man
      } ;

    RelCN cn rs = cn ** {
      s = \\ez,n => cn.s ! enClic ! n ++ rs.s ! agrPesP3 n ;
      } ;

    AdvCN cn ad = cn ** {s = \\ez,n => cn.s ! aEzafa ! n ++ ad.s} ;

    SentCN cn sc = cn ** {s = \\ez,n => cn.s ! ez ! n ++ sc.s} ;

    ApposCN cn np = cn ** {s = \\ez,n => cn.s ! ez ! n ++ np.s ! NPC aEzafa ; definitness = True} ; -- ezafa form of city to be used

}
