--# -path=.:../abstract:../common:
concrete NounGer of Noun = CatGer ** open ResGer, MorphoGer, Prelude in {

  flags optimize=all_subs ;

    -- HL 21.7.2022: the dropping of DefArt in Prep+DefArt works by selecting from
    -- np.s via b = det.hasDefArt = True the forms without det.s and from prep.s
    -- the preposition glued with definite article singular, depending on gender, case.

  lin
    DetCN det cn = {
      s = \\b,c => det.s ! b ! cn.g ! c ++ cn.s ! (adjfCase det.a c) ! det.n ! c ++ cn.adv ;
      a = agrgP3 cn.g det.n ;
      -- isLight = det.isDef ;  -- ich sehe den Mann nicht vs. ich sehe nicht einen Mann
      -- HL 6/2019 (but:) sehe (die|einige) Männer nicht; don't see a|no man = sehe keinen Mann
      w = case det.isDef of { True => case det.hasDefArt of { True => WDefArt ;
                                                              _ => WLight } ;
                              _ => WHeavy } ;
      rc = cn.rc ! det.n ;
      ext = cn.ext
      } ;

    DetNP det = { -- more genders in ExtraGer -- HL: der+er,den+en ; der drei,den drei+en
      s = \\b,c => det.sp ! b ! Neutr ! c ;
      a = agrP3 det.n ;
      -- isPron = False ; -- HL 6/2019: don't apply pronoun switch: ich gebe ihr das  vs. ich gebe es ihr
      w = case det.isDef of { True => WLight ; _ => WHeavy } ;
      rc, ext = []
      } ;

    UsePN pn = {
      s = \\_,c => pn.s ! c ;
      a = agrgP3 pn.g pn.n ;
      w = WLight ; -- means: this is not a heavy NP, but comes before negation
      rc, ext = [] -- Pron => Light, HL 6/2019: to regulate Pron/NonPronNP order
      } ;

    UsePron pron = {
      s = \\_,c => pron.s ! NPCase c ;
      a = pron.a ;
      w = WPron ;
      rc, ext = []
      } ;

    PredetNP pred np = 
      let ag = case pred.a of {PAg n => agrP3 n ; _ => np.a} in np ** {
        s = \\b,c0 =>
          let c = case pred.c.k of {NoCase => c0 ; PredCase k => k} in
          pred.s ! numberAgr ag ! genderAgr np.a ! c0 ++ pred.c.p ++ np.s ! b ! c ;
        a = ag ;
        w = WHeavy
        } ;

    PPartNP np v2 = np ** {
      s = \\b,c => np.s ! b ! c ++ embedInCommas (v2.s ! VPastPart APred) ; --- invar part
      w = WHeavy 
      } ;
    -- SS: "eine erfolgreiche Frau, geliebt von vielen,"  but only with v2 not possible in German?
    -- HL: PPartNP np vps|vp: "der Autor, heute vergessen" , "der Mond, gerade aufgegangen,"
	
    AdvNP np adv = np ** {
      s = \\b,c => np.s ! b ! c ++ adv.s ;
      w = WHeavy 
      } ;

    ExtAdvNP np adv = np ** {
      s = \\b,c => np.s ! b ! c ++ embedInCommas adv.s ;
      w = WHeavy 
      } ;

    DetQuantOrd quant num ord = 
      let 
        n = num.n ;
        a = quant.a
      in {
        s  = \\b,g,c => quant.s  ! b ! num.isNum ! n ! g ! c ++ num.s!g!c
                        ++ ord.s ! agrAdj g (adjfCase a c) n c ;
        sp = \\b,g,c => quant.sp ! b ! num.isNum ! n ! g ! c ++ num.s!g!c
                        ++ ord.s ! agrAdj g (adjfCase quant.aPl c) n c ;
        n = n ;
        a = case n of {Sg => a ; Pl => quant.aPl} ;
        isDef = case <quant.a, quant.aPl> of {<Strong,Strong> => False ; _ => True} ;
        hasDefArt = quant.hasDefArt ;
        } ;

    DetQuant quant num = 
      let 
        n = num.n ;
        a = quant.a ;
        b = andB quant.hasDefArt (case num.n of {Sg => True ; _ => False})
      in {
        s  = \\b,g,c => quant.s  ! b ! num.isNum ! n ! g ! c ++ num.s ! g ! c ;
        sp = \\_,g,c => quant.sp ! False ! num.isNum ! n ! g ! c ++ num.s ! g ! c ;
        -- HL: der+er,den+en ; der drei,den drei+en
        n = n ;
        a = case n of {Sg => a ; Pl => quant.aPl} ;
        isDef = case <quant.a, quant.aPl> of {<Strong,Strong> => False ; _ => True} ;
        hasDefArt = quant.hasDefArt ;
        } ;


    PossPron p = {
      s  = \\_,_,n,g,c => p.s ! NPPoss (gennum g n) c ;
      sp = \\_,_,n,g,c => p.s ! NPPoss (gennum g n) c ;
      a = Strong ;
      aPl = Weak ;
      hasDefArt = False ;
      } ;

    NumCard n = n ** {isNum = True} ;

    NumPl = {s = \\g,c => []; n = Pl ; isNum = False} ; 
    NumSg = {s = \\g,c => []; n = Sg ; isNum = False} ; 

    NumDigits numeral = {s = \\g,c => numeral.s ! NCard g c; n = numeral.n } ;
    OrdDigits numeral = {s = \\af => numeral.s ! NOrd af} ;

    NumFloat dig1 dig2 = {s = \\g,c => dig1.s ! invNum ++ BIND ++ "." ++ BIND ++ dig2.s ! NCard g c ; n = Pl } ;
    NumDecimal numeral = {s = \\g,c => numeral.s ! NCard g c; n = numeral.n } ;

    NumNumeral numeral = {s = \\g,c => numeral.s ! NCard g c; n = numeral.n } ;
    OrdNumeral numeral = {s = \\af => numeral.s ! NOrd af} ;

    AdNum adn num = {s = \\g,c => adn.s ++ num.s!g!c; n = num.n } ;

    OrdSuperl a = {s = a.s ! Superl} ;

    OrdNumeralSuperl n a = {s = \\af => n.s ! NOrd APred ++ Predef.BIND ++ a.s ! Superl ! af} ; -- drittbeste

    DefArt = {
      s = table{True => \\_,n,g,c => [] ; -- definite article dropped
                False => \\_,n,g,c => artDef ! (gennum g n) ! c} ;
      sp = \\_,_,n,g,c  => case <n,c> of {
                             <Pl,Dat> => "denen" ; -- HL 6/2019
                             <Pl,Gen> => "derer" ; -- HL 6/2019
                             _ => artDef ! (gennum g n) ! c } ;  -- von den+en
      a, aPl = Weak ;
      hasDefArt = True 
      } ;

    IndefArt = {
      s = \\_ => table {
        True => \\_,_,c => [] ;
        False => table {
          Sg => \\g,c => "ein" + pronEnding ! GSg g ! c ;
          Pl => \\_,c => []
          }
        } ; 
      sp = \\_ => table {
        True => \\_,_,c => [] ;
        False => table {
          Sg => \\g,c => (detUnlikeAdj False Sg "ein").s ! g ! c ;
          Pl => \\_,c => caselist "einige" "einige" "einigen" "einiger" ! c
          }
        } ;
      a, aPl = Strong ;
      hasDefArt = False
      } ;

    MassNP cn = {
      s = \\_,c => cn.s ! Strong ! Sg ! c ++ cn.adv ;
      a = agrgP3 cn.g Sg ;
      w = WLight ; -- ich trinke Bier nicht vs. ich trinke kein Bier
      rc = cn.rc ! Sg ;
      ext = cn.ext ;
      hasDefArt = False
      } ;

    UseN, UseN2 = \n -> {
      s = \\_ => n.s ;
      g = n.g ;
      rc = \\_ => [] ;
      ext,adv = [] 
      } ;

    ComplN2 f x = {
      s = \\_,n,c => f.s ! n ! c ++ appPrepNP f.c2 x ;
      g = f.g ;
      rc = \\_ => [] ;
      ext,adv = []
      } ;

    ComplN3 f x = {
      s = \\n,c => f.s ! n ! c ++ appPrepNP f.c2 x ;
      co = f.co ++ appPrepNP f.c2 x ; ---- should not occur at all; the abstract syntax is problematic in giving N2
      uncap = {
        s = \\n,c => f.uncap.s ! n ! c ++ appPrepNP f.c2 x ;
        co = f.uncap.co ++ appPrepNP f.c2 x ; ---- should not occur at all; the abstract syntax is problematic in giving N2
       } ;
      g = f.g ; 
      c2 = f.c3 ;
      } ;

    Use2N3 f = f ;

    Use3N3 f = f ** {
      c2 = f.c3;
      } ;

    AdjCN ap cn = 
      let 
        g = cn.g 
      in cn ** {
        s = \\a,n,c => 
               preOrPost ap.isPre
                 (ap.c.p1 ++ ap.c.p2 ++ ap.s ! agrAdj g a n c ++ ap.ext)
                 (cn.s ! a ! n ! c) ;
        g = g
        } ;

 
    RelCN cn rs = cn ** {rc = \\n => (cn.rc ! n ++ embedInCommas (rs.s ! RGenNum (gennum cn.g n)))} ;
    ---- another layer of embedInCommas needed if there is a non-empty rc
    
    RelNP np rs = np ** {
      rc = np.rc ++ embedInCommas (rs.s ! RGenNum (gennum (genderAgr np.a) (numberAgr np.a))) ;
      w = case isPron np of { True => WLight ; _ => np.w } 
      } ;

    SentCN cn s = cn ** {ext = cn.ext ++ embedInCommas s.s} ;

    AdvCN cn a = cn ** {adv = cn.adv ++ a.s} ;

    ApposCN  cn np = let g = cn.g in cn ** {
      s = \\a,n,c => cn.s ! a ! n ! c ++ np.s ! False ! c ++ bigNP np } ;

    PossNP cn np = cn ** {
      s = \\a,n,c => cn.s ! a ! n ! c ++ appPrep vonDat (np.s ! False) ++ bigNP np } ;

    PartNP cn np = case np.w of {
      WPron => cn ** {s = \\a,n,c => cn.s ! a ! n ! c ++ appPrep vonDat (np.s ! False) ++ np.rc} ;
      _     => cn ** {s = \\a,n,c => cn.s ! a ! n ! c ++ np.s ! False ! Gen ++ np.ext ++ np.rc}
        };         -- glass of wine

    CountNP det np = -- drei der Kinder | drei von den Kindern -- HL 7/22, ad-hoc TODO
                     -- det or numeral? np or rather (DefArt +) cn?  drei (einiger Kinder) ?
      let g : Gender = genderAgr np.a
      in {
        s = \\b,c => det.s ! b ! g ! c ++ appPrepNP vonDat np ++ bigNP np ;
        a = agrgP3 g det.n ;
        w = case det.isDef of { True => WLight ; _ => WHeavy } ;
        rc = np.rc ;
        ext = np.ext
      } ;

    AdjDAP dap ap = -- the large (one)  -- HL 8/22 der auf dich stolze; die ihm treue; der so dumme, infzu
      {s, sp  = \\g,c => dap.s ! g ! c ++ ap.c.p1 ++ ap.c.p2 ++ ap.s ! (AMod (gennum g dap.n) c) ++ ap.ext ;
       a = dap.a ; n = dap.n ; isDef = dap.isDef ; hasDefArt = dap.hasDefArt } ;

    DetDAP det = {
      s = \\g,c => det.s ! False ! g ! c ;  -- HL 7/22 todo: check
      sp = \\g,c => det.sp ! False ! g ! c ;
      n = det.n ; a = det.a ; isDef = det.isDef ; hasDefArt = det.hasDefArt
      } ;

    QuantityNP dig m = {
      s = \\_,c => preOrPost m.isPre m.s (dig.s ! invNum) ;
      a = agrP3 Pl ;
      w = WLight ;
      rc = "" ;
      ext = "" ;
      } ;

    QuantityFloatNP dig1 dig2 m = {
      s = \\_,c => preOrPost m.isPre m.s (dig1.s ! invNum ++ BIND ++ "." ++ BIND ++ dig2.s ! invNum) ;
      a = agrP3 Pl ;
      w = WLight ;
      rc = "" ;
      ext = "" ;
      } ;

}
