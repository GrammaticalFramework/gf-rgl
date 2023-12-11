incomplete concrete NounRomance of Noun =
   CatRomance ** open CommonRomance, ResRomance, Prelude in {

  flags optimize=all_subs ; coding = utf8 ;

  lin
    DetCN det cn =
      let
        g = cn.g ;
        n = det.n
      in heavyNPpol det.isNeg {
        s = \\c => det.s ! g ! c ++ cn.s ! n ++ det.s2 ! g ;
        a = agrP3 g n ;
        hasClit = False
        } ;

    UsePN = pn2np ;

    UsePron p = p ** {isNeg = False} ;

    PredetNP pred np =
      let agr = complAgr np.a in
      heavyNPpol np.isNeg {
        s = \\c => pred.s ! agr ! c ++ (np.s ! pred.c).ton ;
        a = case pred.a of {PAg n => agrP3 agr.g n ; _ => np.a} ;
        hasClit = False
      } ;

    PPartNP np v2 =
      let agr = complAgr np.a in
      heavyNPpol np.isNeg {
        s = \\c => (np.s ! c).ton ++ v2.s ! VPart agr.g agr.n ;
        a = np.a ;
        hasClit = False
      } ;

    RelNP np rs = heavyNPpol np.isNeg {
      s = \\c => (np.s ! c).ton ++ rs.s ! Indic ! np.a ;
      a = np.a ;
      hasClit = False
      } ;

    AdvNP np adv = heavyNPpol np.isNeg {
      s = \\c => (np.s ! c).ton ++ adv.s ;
      a = np.a ;
      hasClit = False
      } ;

    ExtAdvNP np adv = heavyNPpol np.isNeg {
      s = \\c => (np.s ! c).ton ++ embedInCommas adv.s ;
      a = np.a ;
      hasClit = False
      } ;

    DetQuantOrd quant num ord = {
      s = \\g,c => quant.s ! num.isNum ! num.n ! g ! c ++ num.s ! g ++
                   ord.s ! aagr g num.n ;
      s2 = \\g => quant.s2 ++ ord.s2 ! aagr g num.n ;
      sp  = \\g,c => quant.s ! num.isNum ! num.n ! g ! c ++ num.s ! g ++
                     ord.s ! aagr g num.n ++ ord.s2 ! aagr g num.n ;
      spn = \\c => quant.s ! num.isNum ! num.n ! Masc ! c ++ num.s ! Masc ++
                   ord.s ! aagr Masc num.n ++ ord.s2 ! aagr Masc num.n ;
      n = num.n ;
      isNeg = quant.isNeg
      } ;

    DetQuant quant num = {
      s  = \\g,c => quant.s ! num.isNum ! num.n ! g ! c ++ num.s ! g ;
      sp = \\g,c => case num.isNum of {
        True  => quant.s ! True ! num.n ! g ! c ++ num.s ! g ;
        False => quant.sp ! num.n ! g ! c ++ num.s ! g
        } ;
      spn= \\c => case num.isNum of {
        True  => quant.s ! True ! num.n ! Masc ! c ++ num.s ! Masc ;
        False => quant.spn ! c ++ num.s ! Masc
        } ;
      s2 = \\_ => quant.s2 ;
      n  = num.n ;
      isNeg = quant.isNeg
      } ;

    DetNP det =
      let
        g = Masc ;  ---- Fem in Extra
        n = det.n
      in heavyNPpol det.isNeg {
        s = det.sp ! g ;
        a = agrP3 g n ;
        hasClit = False
        } ;

    PossPron p = {
      s = \\_,n,g,c => possCase g n c ++ p.poss ! n ! g ; ---- il mio!
      sp = \\ n,g,c => possCase g n c ++ p.poss ! n ! g ; ---- not for Fre
      spn= \\ c => possCase Masc Sg c ++ p.poss ! Sg ! Masc ; ---- not for Fre
      s2 = [] ;
      isNeg = False
      } ;

    NumSg = {s = \\_ => [] ; isNum = False ; n = Sg} ;
    NumPl = {s = \\_ => [] ; isNum = False ; n = Pl} ;

    NumCard n = n ** {isNum = True} ;

    NumDigits nu = {s = \\g => nu.s ! NCard g ; n = nu.n} ;
    OrdDigits nu = {s = \\a => nu.s ! NOrd a.g a.n ; s2 = \\_ => []} ;

    NumDecimal nu = {s = \\g => nu.s ! NCard g ; n = nu.n} ;

    NumNumeral nu = {s = \\g => nu.s ! NCard g ; n = nu.n} ;
    OrdNumeral nu = {s = \\a => nu.s ! NOrd a.g a.n ; s2 = \\_ => []} ;

    AdNum adn num = {s = \\a => adn.s ++ num.s ! a ; isNum = num.isNum ; n = num.n} ;

    OrdSuperl adj =
      case <adj.isDeg, superlCanBePost> of {
        <True, _>      => { s  = \\a => adj.compar ! aagr2compar a ;
                            s2 = \\_ => []
                          } ;
        <False, True>  => { s  = \\_ => [] ;
                            s2 = \\a => piuComp ++ adj.s ! genNum2Aform a.g a.n
                          } ;
        <False, False> => { s  = \\a => piuComp ++ adj.s ! genNum2Aform a.g a.n ;
                            s2 = \\_ => []
                          }
      } ;

    OrdNumeralSuperl num adj =
      case <adj.isDeg, superlCanBePost> of {
        <True, _>      => { s  = \\a => num.s ! NOrd a.g a.n ++ adj.compar ! aagr2compar a ;
                            s2 = \\_ => []
                          } ;
        <False, True>  => { s  = \\a => num.s ! NOrd a.g a.n ;
                            s2 = \\a => piuComp ++ adj.s ! genNum2Aform a.g a.n
                          } ;
        <False, False> => { s  = \\a => num.s ! NOrd a.g a.n ++ piuComp ++ adj.s ! genNum2Aform a.g a.n ;
                            s2 = \\_ => []
                          }
      } ;

    DefArt = {
      s = \\_,n,g,c => artDef False g n c ;
      sp = \\n,g,c => artDef True g n c ;
      spn= \\c => artDef True Masc Sg c ;
      s2 = [] ;
      isNeg = False
      } ;

    IndefArt = {
      s = \\b,n,g,c => if_then_Str b (prepCase c) (artIndef False g n c) ;
      sp = \\n,g,c => artIndef True g n c ;
      spn= \\c => artIndef True Masc Sg c ;
      s2 = [] ;
      isNeg = False
      } ;

    MassNP cn = let
        g = cn.g ;
        n = Sg
      in heavyNP {
        s = table {Nom => artDef False g n Nom ++ cn.s ! n ; c => partitive g c ++ cn.s ! n} ; -- le vin est bon ; je bois du vin
        a = agrP3 g n ;
        hasClit = False ;
        isNeg = False
        } ;

-- This is based on record subtyping.

    UseN, UseN2 = \noun -> noun ;

    Use2N3 f = f ;

    Use3N3 f = f ** {c2 = f.c3} ;

    ComplN2 f x = {
      s = \\n => f.s ! n ++ appCompl f.c2 x ;
      g = f.g ;
      relType = f.relType
      } ;

    ComplN3 f x = {
      s = \\n => f.s ! n ++ appCompl f.c2 x ;
      g = f.g ;
      relType = f.relType ;
      c2 = f.c3
      } ;

    AdjCN ap cn =
      let
        g = cn.g
      in {
        s = \\n => preOrPost ap.isPre (ap.s ! genNumPos2Aform g n ap.isPre) (cn.s ! n) ;
        g = g ;
        } ;

    RelCN cn rs = let g = cn.g in {
      s = \\n => cn.s ! n ++ rs.s ! Indic ! agrP3 g n ; --- mood
      g = g
      } ;
    SentCN  cn sc = let g = cn.g in {
      s = \\n => cn.s ! n ++ sc.s ! genitive ;  -- raison de dormir
      g = g
      } ;
    AdvCN  cn sc = let g = cn.g in {
      s = \\n => cn.s ! n ++ sc.s ;
      g = g
      } ;

    ApposCN  cn np = let g = cn.g in {
      s = \\n => cn.s ! n ++ (np.s ! Nom).ton ;
      g = g
      } ;

    PossNP cn np = {
      s = \\n => cn.s ! n ++ appCompl {s = [] ; c = genitive ; isDir = False} np ;
      g = cn.g ;
      } ;

    PartNP cn np = {
      s = \\n => cn.s ! n ++ appCompl {s = [] ; c = genitive ; isDir = False} np ;
      g = cn.g ;
      } ;

    CountNP det np = heavyNPpol np.isNeg
      {s = \\c => det.s ! np.a.g ! c ++ (np.s ! genitive).ton ;
       a = np.a ** {n = det.n} } ;

    AdjDAP det ap = {
      s = \\g,c => det.s ! g ! c ++ ap.s ! genNum2Aform g det.n ;
      n = det.n ;
      s2 = det.s2 ;   -- -ci
      sp = \\g,c => det.s ! g ! c ++ ap.s ! genNum2Aform g det.n ;
      spn= \\c => det.s ! Masc ! c ++ ap.s ! genNum2Aform Masc det.n ;
      isNeg = det.isNeg
      } ;

    DetDAP det = det ;

    QuantityNP n m = heavyNPpol False {
      s = \\c => case <c,m.hasArt> of {
                  <Acc,True>|<CPrep _,True> => artDef False Masc Sg c ++ preOrPost m.isPre m.s (n.s ! NCard Masc);
                  c => prepCase c.p1 ++ preOrPost m.isPre m.s (n.s ! NCard Masc)};
      a = agrP3 Masc n.n ;
      hasClit = False
      } ;
    }

