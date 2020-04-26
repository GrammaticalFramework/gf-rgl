concrete NounGer of Noun = CatGer ** open ResGer, MorphoGer, Prelude in {

  flags optimize=all_subs ;

-- Remark: np.isLight makes ResGer.insertObjNP expensive, for ComplSlash, SlashVP

  lin
    DetCN det cn = {
      s = \\c => det.s ! cn.g ! c ++ 
                 (let k = (prepC c).c in cn.s ! adjfCase det.a k ! det.n ! k ++ cn.adv) ;
      a = agrgP3 cn.g det.n ;  
      -- isLight = det.isDef ;  -- ich sehe den Mann nicht vs. ich sehe nicht einen Mann
      -- isPron = False ;       -- HL 6/2019 (but:) sehe (die|einige) Männer nicht
                                --                  don't see a|no man = sehe keinen Mann
      w = case det.isDef of { True => WLight ; _ => WHeavy } ;
      rc = cn.rc ! det.n ;   
      ext = cn.ext 
      } ;

    DetNP det = {
      s = \\c => det.sp ! Neutr ! c ; -- more genders in ExtraGer -- HL: der+er,den+en ; der drei,den drei+en
      a = agrP3 det.n ;
      -- isLight = det.isDef ;
      -- isPron = False ; -- HL 6/2019: don't apply pronoun switch: ich gebe ihr das  vs. ich gebe es ihr
      w = case det.isDef of { True => WLight ; _ => WHeavy } ;
      rc, ext = []
      } ;

    UsePN pn = {
      s = \\c => usePrepC c (\k -> pn.s ! k) ;
      a = agrgP3 pn.g Sg ;
--      isLight = True ;  -- means: this is not a heavy NP, but comes before negation
--      isPron = False ;  -- HL 6/2019: to regulate Pron/NonPronNP order 
      w = WLight ;  
      rc, ext = []
      } ;

    UsePron pron = {
      s = \\c => usePrepC c (\k -> pron.s ! NPCase k) ;
      a = pron.a ;
      -- isLight = True ;
      -- isPron = True ;
      w = WPron ;
      rc, ext = []
      } ;

    PredetNP pred np = 
      let ag = case pred.a of {PAg n => agrP3 n ; _ => np.a} in np ** {
        s = \\c0 => 
          let c = case pred.c.k of {NoCase => c0 ; PredCase k => k} in
          pred.s ! numberAgr ag ! genderAgr np.a ! c0 ++ pred.c.p ++ np.s ! c ; 
        a = ag ;
        -- isLight = False ;
        -- isPron = False
        w = WHeavy 
        } ;

    PPartNP np v2 = np ** {
      s = \\c => np.s ! c ++ embedInCommas (v2.s ! VPastPart APred) ; --- invar part
--      isPron = False
      w = WHeavy 
      } ;
	{- "eine erfolgreiche Frau, geliebt von vielen,"  but only with v2 not possible in German?
            HL: PPartNP np vps|vp: "der Autor, heute vergessen" , "der Mond, gerade aufgegangen,"
         -}
	
    AdvNP np adv = np ** {
      s = \\c => np.s ! c ++ adv.s ;
      -- isLight = False ;
      -- isPron = False
      w = WHeavy 
      } ;

    ExtAdvNP np adv = np ** {
      s = \\c => np.s ! c ++ embedInCommas adv.s ;
      -- isLight = False ;
      -- isPron = False
      w = WHeavy 
      } ;

    DetQuantOrd quant num ord = 
      let 
        n = num.n ;
        a = quant.a
      in {
        s  = \\g,c => quant.s  ! num.isNum ! n ! g ! c ++ (let k = (prepC c).c in
                        num.s!g!k ++ ord.s ! agrAdj g (adjfCase a k) n k) ;
        sp = \\g,c => quant.sp ! num.isNum ! n ! g ! c ++ (let k = (prepC c).c in
                        num.s!g!k ++ ord.s ! agrAdj g (adjfCase quant.aPl k) n k) ;
        n = n ;
        a = case n of {Sg => a ; Pl => quant.aPl} ;
        isDef = case <quant.a, quant.aPl> of {<Strong,Strong> => False ; _ => True} ;
        } ;

    DetQuant quant num = 
      let 
        n = num.n ;
        a = quant.a
      in {
        s  = \\g,c => quant.s  ! num.isNum ! n ! g ! c ++ (let k = (prepC c).c in
                        num.s!g!k) ;
        sp = \\g,c => quant.sp ! num.isNum ! n ! g ! c ++ (let k = (prepC c).c in
                        num.s!g!k) ; -- HL: der+er,den+en ; der drei,den drei+en
        n = n ;
        a = case n of {Sg => a ; Pl => quant.aPl} ;
        isDef = case <quant.a, quant.aPl> of {<Strong,Strong> => False ; _ => True} ;
        } ;


    PossPron p = {
      s  = \\_,n,g,c => usePrepC c (\k -> p.s ! NPPoss (gennum g n) k) ;
      sp = \\_,n,g,c => usePrepC c (\k -> p.s ! NPPoss (gennum g n) k) ;
      a = Strong ;
      aPl = Weak ;
      } ;

    NumCard n = n ** {isNum = True} ;

    NumPl = {s = \\g,c => []; n = Pl ; isNum = False} ; 
    NumSg = {s = \\g,c => []; n = Sg ; isNum = False} ; 

    NumDigits numeral = {s = \\g,c => numeral.s ! NCard g c; n = numeral.n } ;
    OrdDigits numeral = {s = \\af => numeral.s ! NOrd af} ;

    NumNumeral numeral = {s = \\g,c => numeral.s ! NCard g c; n = numeral.n } ;
    OrdNumeral numeral = {s = \\af => numeral.s ! NOrd af} ;

    AdNum adn num = {s = \\g,c => adn.s ++ num.s!g!c; n = num.n } ;

    OrdSuperl a = {s = a.s ! Superl} ;

    OrdNumeralSuperl n a = {s = \\af => n.s ! NOrd APred ++ Predef.BIND ++ a.s ! Superl ! af} ; -- drittbeste

    DefArt = {
      s = \\_,n,g,c => artDefContr (gennum g n) c ; 
--      sp = \\_,n,g,c  => artDefContr (gennum g n) c ;  ---- deren, denen ...
      sp = \\_,n,g,c  => case <n,c> of {
        <Sg,NPP p> => let sp = prepC c ; gn = gennum g n 
          in sp.s ++ artDef ! gn ! sp.c ;
        <Pl,NPP CInAcc> => let sp = prepC c in sp.s ++ "die" ;
        <Pl,NPP p> => let sp = prepC c ; gn = gennum g n 
          in sp.s ++ (artDef ! gn ! sp.c + "en") ;
        <Pl,NPC Dat> => "denen" ; -- HL 6/2019
        <Pl,NPC Gen> => "derer" ; -- HL 6/2019
        _ => artDefContr (gennum g n) c } ;  -- von den+en
      a, aPl = Weak
      } ;

    IndefArt = {
      s = table {
        True => \\_,_,c => usePrepC c (\k -> []) ;
        False => table {
          Sg => \\g,c => usePrepC c (\k -> "ein" + pronEnding ! GSg g ! k) ;  
          Pl => \\_,c => usePrepC c (\k -> [])
          }
        } ; 
      sp = table {
        True => \\_,_,c => usePrepC c (\k -> []) ;
        False => table {
          Sg => \\g,c => usePrepC c (\k -> (detUnlikeAdj False Sg "ein").s ! g ! NPC k) ;
          Pl => \\_,c => usePrepC c (\k -> caselist "einige" "einige" "einigen" "einiger" ! k)
          }
        } ;
      a, aPl = Strong 
      } ;

    MassNP cn = {
      s = \\c => usePrepC c (\k -> cn.s ! Strong ! Sg ! k) ++ cn.adv ;
      a = agrgP3 cn.g Sg ;
      -- isLight = True ;  -- ich trinke Bier nicht vs. ich trinke kein Bier
      -- isPron = False ;
      w = WLight ;
      rc = cn.rc ! Sg ;
      ext = cn.ext
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
      rc = (np.rc ++ embedInCommas (rs.s ! RGenNum (gennum (genderAgr np.a) (numberAgr np.a)))) ;
      -- isPron = False 
      w = case isPron np of { True => WLight ; _ => np.w } 
      } ;

    SentCN cn s = cn ** {ext = cn.ext ++ embedInCommas s.s} ;

    AdvCN cn a = cn ** {adv = cn.adv ++ a.s} ;

    ApposCN  cn np = let g = cn.g in cn ** {
      s = \\a,n,c => cn.s ! a ! n ! c ++ np.s ! NPC c ++ bigNP np } ;

    PossNP cn np = cn ** {
      s = \\a,n,c => cn.s ! a ! n ! c ++ np.s ! NPP CVonDat ++ bigNP np } ;
}
