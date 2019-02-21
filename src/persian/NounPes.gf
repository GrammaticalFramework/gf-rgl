concrete NounPes of Noun = CatPes ** open ResPes, Prelude in {

  flags optimize=all_subs ;

  lin
    DetCN det cn = cn ** {s = \\mod =>
      case <det.isNum,det.mod> of {
        <False,Bare> => det.s ++ cn.s ! det.n ! mod  ; -- det is not from Pron, retain NPForm.
        <False,X>         => cn.s ! det.n ! X  ++ det.s ; -- det is from Pron, cn is in Ezafe.
        <True,Bare>  => det.s ++ cn.s ! Sg ! mod  ; -- noun modified by a number is invariably singular
        <True,X>          => cn.s ! Sg ! X ++ det.s
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
      s = \\ez => np.s !  Ezafe ++ adv.s
      } ;

    DetQuantOrd quant num ord = {
      s = quant.s ! num.n ++ num.s ++ ord.s ;
      isNum = orB num.isNum ord.isNum ;
      mod = quant.mod ;
      n = num.n
      } ;

    DetQuant quant num = {
      s = quant.s ! num.n ++ num.s;
      isNum = num.isNum;
      mod = quant.mod ;
      n = num.n
      } ;

    DetNP det = {
      s = \\_ => det.s  ; ---- case
      a = agrP3 det.n ;
      hasAdj = False ;
      animacy = Inanimate
      } ;

    PossPron p = {s = \\_ => BIND ++ p.ps ; a = p.a ; mod = Poss} ;

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

    OrdSuperl a = {s = a.s ! Bare ++ taryn; n = Sg ; isNum=False} ; -- check the form of adjective

    DefArt = {s = \\_ => [] ; a = defaultAgr ; mod = Bare} ;
    IndefArt = {s = table { Sg => IndefArticle ; Pl => []} ; a =defaultAgr ; mod = Bare} ;

    MassNP cn = cn ** {
      s = cn.s ! Sg ;
      a = agrP3 Sg
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
      s = \\n,ez => f.s ! n ! Ezafe ++ f.c ++ x.s !  ez ;
      definitness = True ;
      hasAdj = False ;
     };
    ComplN3 f x = f ** {
      s = \\n,ez => f.s ! n ! Ezafe  ++ f.c2 ++ x.s !  ez ;
      c = f.c3;
      definitness = True;
      } ;

    AdjCN ap cn = cn ** {
      s = \\n,ez => cn.s! n! Ezafe ++ ap.s ! ez ; -- check the form of adjective and also cn.s!ez!n changed from cn.s!Ezafe!n to have correct enclicitic form other wise it creats wrong enclictic form of old man
      hasAdj = True
     } ;

    RelCN cn rs = cn ** {
      s = \\n,ez => cn.s ! n! Clitic  ++ rs.s ! agrP3 n ;
      } ;

    AdvCN cn ad = cn ** {s = \\n,ez => cn.s ! n ! Ezafe ++ ad.s} ;

    SentCN cn sc = cn ** {s = \\n,ez => cn.s ! n ! ez ++ sc.s} ;

    ApposCN cn np = cn ** {s = \\n,ez => cn.s ! n ! ez ++ np.s !  Ezafe ; definitness = True} ; -- ezafa form of city to be used

}
