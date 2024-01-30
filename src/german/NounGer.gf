--# -path=.:../abstract:../common:
concrete NounGer of Noun = CatGer ** open ResGer, MorphoGer, Prelude in {

  flags optimize=all_subs ;

    -- HL 21.7.2022: the dropping of DefArt in Prep+DefArt works by selecting from
    -- np.s via b = det.hasDefArt = True the forms without det.s and from prep.s
    -- the preposition glued with definite article singular, depending on gender, case.

  lin
    DetCN det cn = {
      s = \\b,c => det.s ! b ! cn.g ! c ++ cn.s ! det.a ! det.n ! c ++ cn.adv ;
      a = agrgP3 cn.g det.n ;
      -- isLight = det.isDef ;  -- ich sehe den Mann nicht vs. ich sehe nicht einen Mann
      -- HL 6/2019 (but:) sehe (die|einige) Männer nicht; don't see a|no man = sehe keinen Mann
      w = case det.isDef of { True => case det.hasDefArt of { True => WDefArt ;
                                                              _ => WLight } ;
                              _ => WHeavy } ;
      rc = cn.rc ! det.n ;
      ext = cn.ext
      } ;

    DetNP det = { -- more genders in ExtendGer
      s = \\b,c => det.sp ! b ! Neutr ! c ;
      a = agrP3 det.n ;
      -- isPron = False ; -- HL 6/2019: don't apply pronoun switch: ich gebe ihr das  vs. ich gebe es ihr
      w = case det.isDef of { True => case det.hasDefArt of { True => WDefArt ;
                                                              _ => WLight } ;
                              _ => WHeavy } ;
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

  oper
    einziger : AForm => Str = table{AMod gn c => "einzig" + adjEnding ! gn ! c ; _ => "einziges"} ;

  lin
    DetQuantOrd quant num ord = 
      let 
        n = num.n ;
        a = quant.a ;
        d = quant.isDefArt ;
        isCardOne = case n of {Sg => num.isNum ; _ => False} ;
        nums : AForm => Str = \\af => case af of {
          AMod (GSg g) c => case <quant.delCardOne,isCardOne> of {
            <True,True> =>  einziger ! af ;  -- (ein,kein) einziger
            <_,True> => num.sp ! af ;        -- (der,dieser) eine ; (mein) einer
            _ => num.s ! af } ;              -- (die,diese) zwei  ---- todo inflection
          _ => num.s ! APred}
      in {
        s,sp = \\b,g,c => let gn = gennum g n in
          quant.s ! b ! gn ! c ++ nums ! agrAdj a gn c ++ ord.s ! agrAdj a gn c ;
        n = n ;
        a = a ;
        isDef = case a of {Strong => False ; _ => True} ;
        hasDefArt = d
        } ;

    DetQuant quant num = 
      let 
        n = num.n ;
        a = quant.a ;
        d = quant.isDefArt ;
        isCardOne = case n of {Sg => num.isNum ; _ => False} ;
        quantsp : Bool => GenNum => Case => Str =
          case num.isNum of {True => quant.s ; False => \\b => quant.sp} ;
        nums : AForm => Str = \\af => case af of {
          AMod (GSg g) c => case <quant.delCardOne,isCardOne> of {
            <True,True> =>  einziger ! af ;  -- (k)ein einziger, drop cardinal "ein" of num
            <_,True> => num.sp ! af ;        -- (der,dieser) eine ; (mein) einer
            _ => num.s ! af } ;
          AMod GPl c => num.s ! APred ;      -- (den,diesen) zwei(en)  ---- todo: inflection
          APred => num.s ! APred}
      in {
        s = \\b,g,c => let gn = gennum g n in
          quant.s ! b ! gn ! c ++ nums ! agrAdj a gn c ;
        sp = \\b,g,c => let gn = gennum g n in
          quantsp ! b ! gn ! c ++ nums ! agrAdj a gn c ;
        n = n ;
        a = a ;
        isDef = case a of {Strong => False ; _ => True} ;
        hasDefArt = d
      } ;

    PossPron p = {
      s  = \\_,gn,c => p.s ! NPPoss gn c ; -- mein (dritter)
      sp = \\gn,c => p.sp ! PossF gn c ;   -- meiner
      a = Mixed ;
      isDefArt = False ;
      delCardOne = False ;
      } ;

    NumCard n = n ** {
      isNum = True ;
      sp = table {AMod gn c => n.s ! APred ++ BIND ++ adjEnding ! gn ! c ;
                  APred => n.s ! APred}
      } ;

    NumPl = {s,sp = \\_ => []; n = Pl ; isNum = False} ;
    NumSg = {s,sp = \\_ => []; n = Sg ; isNum = False} ;

    NumDigits digits = {s = \\af => digits.s ! NCard af ; n = digits.n} ;
    OrdDigits digits = {s = table{APred => "am" ++ digits.s ! NOrd APred ++ BIND ++ "en" ;
                                  af => digits.s ! NOrd af}} ;

    NumFloat dig1 dig2 = {s = \\g,c => dig1.s ! invNum ++ BIND ++ "." ++ BIND ++ dig2.s ! NCard g c ; n = Pl } ;
    NumDecimal decimal = {s = \\af => decimal.s ! NCard af ; n = decimal.n } ;

    NumNumeral numeral = {s = \\af => numeral.s ! NCard af ; n = numeral.n } ;
    OrdNumeral numeral = {s = table{APred => "am" ++ numeral.s ! NOrd APred ++ BIND ++ "en" ;
                                    af => numeral.s ! NOrd af}} ;

    AdNum adn num = {s = \\af => adn.s ++ num.s ! af ; n = num.n } ;

    OrdSuperl a = {s = table {APred => "am" ++ a.s ! Superl ! APred ;
                              af => a.s ! Superl ! af}} ;

    OrdNumeralSuperl n a =
      {s = table {APred => "am" ++ n.s ! NOrd APred ++ BIND ++ a.s ! Superl ! APred ; -- am drittbesten
                  af => n.s ! NOrd APred ++ BIND ++ a.s ! Superl ! af}                -- drittbeste
      } ;
    DefArt = {
      s = \\b,gn,c => case <b,gn> of {<True,GSg _> => [] ; _ => artDef ! gn ! c} ;
      sp = \\gn,c  => case <gn,c> of {
                            <GSg Masc,Gen> => "dessen" ;
                            <GSg Fem, Gen> => "derer" ;
                            <GSg Neutr,Gen> => "dessen" ;
                            <GPl,Dat> => "denen" ; -- HL 6/2019
                            <GPl,Gen> => "derer" ; -- HL 6/2019
                             _ => artDef ! gn ! c } ;
      a = Weak ;
      isDefArt = True ;
      delCardOne = False ;
      } ;

    IndefArt = {
      s = \\_ => table {GSg g => \\c => "ein" + pronEnding ! (GSg g) ! c ;
                        GPl => \\c => []} ;
      sp = table {GSg g => \\c => "ein" + detEnding ! (GSg g) ! c ;
                  GPl => caselist "einige" "einige" "einigen" "einiger"} ;
      a = MixedStrong ; -- Sg Mixed, Pl Strong
      isDefArt = False ;
      delCardOne = True ;
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
        s = case ap.isPre of { -- HL 1/2023 False only for ap = SentAP ap' sc
          True => \\a,n,c =>   -- besserer cn als a.s2 [instead: cn, besser als a.s2,]
                 (ap.c.p1 ++ ap.c.p2 ++ ap.s ! agrAdj a (gennum g n) c)
            ++   (cn.s ! a ! n ! c) ++ ap.s2 ! c ++ ap.ext ;
          False => \\a,n,c => cn.s ! a ! n ! c ++ -- postnominal ap with sc
            embedInCommas (ap.c.p1 ++ ap.c.p2 ++ ap.s ! APred ++ ap.s2 ! c ++ ap.ext)} ;
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
        s = \\b,c => det.s ! b ! g ! c ++ appPrepNP vonDat np ;
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
