--# -path=.:../abstract:../common:
concrete NounGer of Noun' = CatGer ** open ResGer, MorphoGer, Prelude in {

  flags optimize=all_subs ;

-- Remark: np.isLight makes ResGer.insertObjNP expensive, for ComplSlash, SlashVP

  lin
    DetCN det cn = {
      s = \\c => <(det.s ! cn.g ! c).quant,
                  (det.s ! cn.g ! c).num ++ cn.s ! (adjfCase det.a c) ! det.n ! c ++ cn.adv> ;
      a = agrgP3 cn.g det.n ;
      -- isLight = det.isDef ;  -- ich sehe den Mann nicht vs. ich sehe nicht einen Mann
      -- isPron = False ;       -- HL 6/2019 (but:) sehe (die|einige) Männer nicht
                                --                  don't see a|no man = sehe keinen Mann
      -- w = case det.isDef of { True => WLight' ; _ => WHeavy' } ;
      -- Would be clearer with w:Weight and hasDefArt:Bool with |NP|=|Agr|*3*2 = 108
      -- instead of the more efficient w:Weigth' with |NP|=|Agr|*4 = 18*4 = 54
      w = case det.isDef of { True => case det.hasDefArt of { True => WDefArt ;
                                                              _ => WLight' } ;
                              _ => WHeavy' } ;
      rc = cn.rc ! det.n ;
      ext = cn.ext
      } ;

    DetNP det = { -- more genders in ExtraGer -- HL: der+er,den+en ; der drei,den drei+en
      s = \\c => <(det.sp ! Neutr ! c).quant, (det.sp ! Neutr ! c).num> ; 
      a = agrP3 det.n ;
      -- isLight = det.isDef ;
      -- isPron = False ; -- HL 6/2019: don't apply pronoun switch: ich gebe ihr das  vs. ich gebe es ihr
      w = case det.isDef of { True => WLight' ; _ => WHeavy' } ;
      rc, ext = []
      } ;

    UsePN pn = {
      s = \\c => <[], pn.s ! c> ;
      a = agrgP3 pn.g Sg ;
      w = WLight' ;  -- means: this is not a heavy NP, but comes before negation
      rc, ext = []   -- Pron => Light HL 6/2019: to regulate Pron/NonPronNP order 
      } ;

    UsePron pron = {
      s =  \\c => <[], pron.s ! NPCase c> ;
      a = pron.a ;
      w = WPron' ;
      rc, ext = []
      } ;

    PredetNP pred np = 
      let ag = case pred.a of {PAg n => agrP3 n ; _ => np.a} in np ** {
        s = \\c0 => 
          let c = case pred.c.k of {NoCase' => c0 ; PredCase' k => k} in
          <pred.s ! numberAgr ag ! genderAgr np.a ! c0 ++ pred.c.p ++ (np.s ! c).p1, (np.s ! c).p2> ; 
        a = ag ;
        w = WHeavy' 
        } ;

    PPartNP np v2 = np ** {
      s = incr2 np.s (embedInCommas (v2.s ! VPastPart APred)) ; --- invar part
      w = WHeavy'
      } ;
	{- "eine erfolgreiche Frau, geliebt von vielen,"  but only with v2 not possible in German?
            HL: PPartNP np vps|vp: "der Autor, heute vergessen" , "der Mond, gerade aufgegangen,"
         -}
	
    AdvNP np adv = np ** {
      s = incr2 np.s adv.s ;
      w = WHeavy'
      } ;

    ExtAdvNP np adv = np ** {
      s = incr2 np.s (embedInCommas adv.s) ;
      w = WHeavy'
      } ;

    -- HL 21.7.2022: the dropping of DefArt in Prep+DefArt via hasDefArt works by splitting the
    -- np.s into det':Str and cn:Str and (det.s ! g ! n) into {quant:Str; num:Str}, so that in
    -- PrepNP in_Prep np we can replace "in" + "das" by "im" to get
    --    PrepNP in_Prep (DetCN (DetQuant DefArt NumSg) (warme Meer)) => im warmen Meer
    -- But parsing "im warmen Meer" results in a 
    --    PrepNP in_Prep (DetCN (DetQuant ? NumSg) (AdjCN ... ))
    -- because (DetQuant.s!g!c).quant is ignored, but the .num is part of (np.s!c).p2.
    -- To avoid the metavariable ?, we have to make Det.s and NP.s depend on t:PrepType = isPrep.

    DetQuantOrd quant num ord = 
      let                       -- does not work, since here DefArt is combined with ord
        n = num.n ;             --
        a = quant.a
      in {
        s  = \\g,c => {quant = quant.s  ! num.isNum ! n ! g ! c;
                       num = num.s!g!c ++ ord.s ! agrAdj g (adjfCase a c) n c} ;
        sp = \\g,c => {quant = quant.sp ! num.isNum ! n ! g ! c;
                       num = num.s!g!c ++ ord.s ! agrAdj g (adjfCase quant.aPl c) n c} ;
        n = n ;
        a = case n of {Sg => a ; Pl => quant.aPl} ;
        isDef = case <quant.a, quant.aPl> of {<Strong,Strong> => False ; _ => True} ;
        hasDefArt = quant.hasDefArt ;
        } ;

    DetQuant quant num =
      let
        n = num.n ;
        a = quant.a
      in {
        s  = \\g,c => {quant = quant.s  ! num.isNum ! n ! g ! c ; num = num.s!g!c} ;
        sp = \\g,c => {quant = quant.sp ! num.isNum ! n ! g ! c ; num = num.s!g!c} ;
        -- HL: der+er,den+en ; der drei,den drei+en
        n = n ;
        a = case n of {Sg => a ; Pl => quant.aPl} ;
        isDef = case <quant.a, quant.aPl> of {<Strong,Strong> => False ; _ => True} ;
        hasDefArt = quant.hasDefArt ;
        } ;

    PossPron p = {
      s  = \\_,n,g,c => p.s ! NPPoss (gennum g n) c ;
      sp = \\_,n,g,c => p.s ! NPPoss (gennum g n) c ;
      a = Strong ;
      aPl = Weak ;
      hasDefArt = False ;
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
      s = \\_,n,g,c => artDef ! (gennum g n) ! c ;
      sp = \\_,n,g,c  => case <n,c> of {
        <Pl,Dat> => "denen" ; -- HL 6/2019
        <Pl,Gen> => "derer" ; -- HL 6/2019
        _ => artDef ! (gennum g n) ! c } ;
      a, aPl = Weak ;
      hasDefArt = True
      } ;
    IndefArt = {
      s = table {
        True => \\_,_,c => [] ;
        False => table {
          Sg => \\g,c => "ein" + pronEnding ! GSg g ! c ;  
          Pl => \\_,c => []
          }
        } ; 
      sp = table {
        True => \\_,_,c => [] ;
        False => table {
          Sg => \\g,c => (detUnlikeAdj' False Sg "ein").s ! g ! c ;
          Pl => \\_,c => caselist "einige" "einige" "einigen" "einiger" ! c
          }
        } ;
      a, aPl = Strong ;
      hasDefArt = False
      } ;

    MassNP cn = {
      s = \\c => <[], cn.s ! Strong ! Sg ! c ++ cn.adv> ;
      a = agrgP3 cn.g Sg ;
      -- isLight = True ;  -- ich trinke Bier nicht vs. ich trinke kein Bier
      -- isPron = False ;
      w = WLight' ;
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
      s = \\_,n,c => f.s ! n ! c ++ appPrepNP' f.c2 x ;
      g = f.g ;
	  rc = \\_ => [] ;
	  ext,adv = []
      } ;

    ComplN3 f x = {
      s = \\n,c => f.s ! n ! c ++ appPrepNP' f.c2 x ;
      co = f.co ++ appPrepNP' f.c2 x ; ---- should not occur at all; the abstract syntax is problematic in giving N2
      uncap = {
        s = \\n,c => f.uncap.s ! n ! c ++ appPrepNP' f.c2 x ;
        co = f.uncap.co ++ appPrepNP' f.c2 x ; ---- should not occur at all; the abstract syntax is problematic in giving N2
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
      w = case isPron' np of { True => WLight' ; _ => np.w }
      } ;

    SentCN cn s = cn ** {ext = cn.ext ++ embedInCommas s.s} ;

    AdvCN cn a = cn ** {adv = cn.adv ++ a.s} ;

    ApposCN  cn np = let g = cn.g in cn ** {
      s = \\a,n,c => cn.s ! a ! n ! c ++ (np.s ! c).p1 ++ (np.s ! c).p2 ++ bigNP' np } ;

    -- PossNP cn np = cn ** {
    --   s = \\a,n,c => cn.s ! a ! n ! c ++ np.s ! NPP CVonDat ++ bigNP np } ;
    PossNP cn np = cn ** {
      s = \\a,n,c => cn.s ! a ! n ! c ++ appPrep2' vonDat' np.s ++ bigNP' np } ;  -- HL, ad hoc

    -- PartNP cn np = todo  -- glass of wine

    CountNP det np = -- drei der Kinder | drei von den Kindern -- HL 7/22, ad-hoc
                     -- det or numeral? np or rather (DefArt +) cn?  drei (einiger Kinder) ?
      let g = genderAgr np.a
      in {
        s = \\c => det.s ! g ! c ++ np.s ! NPP CVonDat ++ bigNP np ;
        a = agrgP3 g det.n ;
        w = case det.isDef of { True => WLight ; _ => WHeavy } ;
        rc = np.rc ;
        ext = np.ext 
      } ;

--    AdjDAP dap adj = ?? TODO  -- the large (one)

    DetDAP det = {s = \\g,c => (det.s ! g ! c).quant ++ (det.s ! g ! c).num ;  -- HL 7/22
                  sp = \\g,c => (det.sp ! g ! c).quant ++ (det.sp ! g ! c).num ;
                  n = det.n ; a = det.a ; isDef = det.isDef ; hasDefArt = det.hasDefArt} ;
  oper
    incr2 : (Case => Str * Str) -> Str -> (Case => Str * Str) = \tab,str ->
      \\c => <(tab ! c).p1, (tab ! c).p2 ++ str> ;

}
