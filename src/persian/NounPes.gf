concrete NounPes of Noun = CatPes ** open ResPes, Prelude in {

  flags optimize=all_subs ;

  lin
    DetCN det cn = cn ** {s = \\npc =>
      case <det.isNum,det.fromPron,npc> of {
        <False,False,NPC ez> => det.s ++ cn.s ! ez ! det.n ; -- det is not from Pron, retain NPForm.
        <False,True>         => cn.s ! aEzafa ! det.n ++ det.s ; -- det is from Pron, cn is in aEzafa.
        <True,False,NPC ez>  => det.s ++ cn.s ! ez ! Sg ; -- noun modified by a number is invariably singular
        <True,True>          => cn.s ! aEzafa ! Sg ++ det.s
      } ;
      a = agrP3 det.n ;
      } ;

    UsePN pn = pn ** {s = \\_ => pn.s ; a = agrP3 Sg ; hasAdj = False} ;
    UsePron p = p ** {s = \\_ => p.s ; animacy = Animate ; hasAdj = False} ;

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
      a = agrP3 det.n ;
      hasAdj = False ;
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

    DefArt = {s = \\_ => [] ; a = defaultAgr ; fromPron = False} ;
    IndefArt = {s = table { Sg => IndefArticle ; Pl => []} ; a =defaultAgr ; fromPron = False} ;

    MassNP cn = cn ** {s =\\c => case c of {
      NPC bEzafa => cn.s ! bEzafa ! Sg ;
      NPC aEzafa => cn.s ! aEzafa ! Sg ;
      NPC enClic => cn.s ! enClic ! Sg
      };
      a = agrP3 Sg ;
      } ;

    UseN n = n ** {hasAdj=False};
    UseN2 n = n ** {hasAdj=False};

    Use2N3 f = f ** {
      c = f.c2;
      definitness = True
      } ;

   Use3N3 f = f ** {
      c = f.c3;
      definitness = True
      } ;

    ComplN2 f x = f ** {
      s = \\ez,n => f.s ! ez ! n  ++ f.c ++ x.s ! NPC ez ;
      definitness = True ;
      hasAdj = False ;
     };
    ComplN3 f x = f ** {
      s = \\ez,n => f.s ! ez ! n ++ f.c2 ++ x.s ! NPC ez ;
      c = f.c3;
      definitness = True;
      } ;

    AdjCN ap cn = cn ** {
      s = \\ez,n => cn.s ! aEzafa ! n ++ ap.s ! ez ; -- check the form of adjective and also cn.s!ez!n changed from cn.s!aEzafa!n to have correct enclicitic form other wise it creats wrong enclictic form of old man
      hasAdj = True
     } ;

    RelCN cn rs = cn ** {
      s = \\ez,n => cn.s ! enClic ! n ++ rs.s ! agrP3 n ;
      } ;

    AdvCN cn ad = cn ** {s = \\ez,n => cn.s ! aEzafa ! n ++ ad.s} ;

    SentCN cn sc = cn ** {s = \\ez,n => cn.s ! ez ! n ++ sc.s} ;

    ApposCN cn np = cn ** {s = \\ez,n => cn.s ! ez ! n ++ np.s ! NPC aEzafa ; definitness = True} ; -- ezafa form of city to be used

}
