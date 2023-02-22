--# -path=.:../prelude:../common:../abstract
--# -coding=utf8

-- A Polish verb Resource Morphology 
--
-- Adam Slaski, 2009 <adam.slaski@gmail.com>
--
resource MorphoVerbLit = ResLit ** open Prelude, CatLit, (Predef=Predef), (Adj=ParadigmsAdjectiveLit), MorphoAdjectiveLit in {

     flags  coding=utf8; 

-- Fonctions de complémentation/formation du noyau prédicatif utilisées dans SentenceLit
-- Pour le lituanien on peut sans doute simplifier

oper

  PresForms : Type = {
    prsg1, prsg2, pr3, prpl1, prpl2 : Str
    } ;

  PastForms : Type = {
    psg1, psg2, p3, ppl1, ppl2 : Str
    } ;

  PastFreqForms : Type = {
    pfsg1, pfsg2, pf3, pfpl1, pfpl2 : Str
    } ;

  FutForms : Type = {
    fsg1, fsg2, f3, fpl1, fpl2 : Str
    } ;

  HypForms : Type = {
    csg1, csg2, c3, cpl1, cpl2 : Str
    } ;

  ImperForms : Type = {
    isg2, ipl1, ipl2 : Str
    } ;

  GerundForms : Type = {
    ms, fs, mp, fp : Str
    } ;

  mkVerb : Str -> Str -> Str -> Verb = 
	 \infForm, presForm, pastForm -> 
	 let presCore = getPresCore presForm in 
	 let pastCore = getPastCore pastForm in 
	 let reflStatus = getReflStatus infForm in 
	 let conjClass = getConjClass presCore.p2 pastCore.p2 in 
         {
	   forms = mkVerbTables reflStatus (getInfStem infForm) presCore.p1 pastCore.p1 conjClass ;
	   refl = reflStatus;
	   asp = Dual;
         -- a revoir 
	   passPastPart = mkPassiveParticipleTable (getInfStem infForm) ;

	   actPastPart = mkActivePastParticipleTable pastCore.p1 ;
	   actPastFreqPart = mkActivePastParticipleTable ((getInfStem infForm) + "dav") ;
	   actPresPart = mkActivePresParticipleTable presCore ;
	   actFutPart = mkActivePresParticipleTable <(getInfStem infForm) + "s",PR_IA> ;
	 };

  mkCopulaVerb : Verb = 
         {
	   forms = mkCopulaVerbTables ;
	   refl = Norefl;
	   asp = Dual;
         -- a revoir 
	   passPastPart = mkPassiveParticipleTable "bū" ;

	   actPastPart = mkActivePastParticipleTable "buv" ;
	   actPastFreqPart = mkActivePastParticipleTable ("bū" + "dav") ;
	   actPresPart = mkActivePresParticipleTable <"es",PR_A> ;
	   actFutPart = mkActivePresParticipleTable <"bū" + "s",PR_IA> ;
	 };


-- Incomplete (Infixed refl not obtained)
  getReflStatus : Str -> ReflStatus = 
    \str -> case str of {
      _ + "tis" => Postfix ;
      _ => Norefl 
    };

  oper mkPassiveParticipleTable : Str -> AdjTable;
  oper mkPassiveParticipleTable infStem = record2table (adj1aModel (infStem + "t")) ;

  oper mkActivePastParticipleTable : Str -> AdjTable;
  oper mkActivePastParticipleTable stem = 
     record2table { 
       msnom = stem + "ęs" ;
       msacc = stem + "usį" ;
       msgen = stem + "usio" ;
       msins = stem + "usiu" ;
       msdat = stem + "usiam" ;
       msloc = stem + "usiame" ;

       mpnom = stem + "ę" ;
       mpacc = stem + "usius" ;
       mpgen = stem + "usių" ;
       mpins = stem + "usiais" ;
       mpdat = stem + "usiems" ;
       mploc = stem + "usiuose" ;

       fsnom = stem + "usi" ;
       fsacc = stem + "usią" ;
       fsgen = stem + "usios" ;
       fsins = stem + "usia" ;
       fsdat = stem + "usiai" ;
       fsloc = stem + "usioje" ;

       fpnom = stem + "usios" ;
       fpacc = stem + "usias" ;
       fpgen = stem + "usių" ;
       fpins = stem + "usiomis" ;
       fpdat = stem + "usioms" ;
       fploc = stem + "usiose" ;

       nnom = stem + "ę" ;
     } ;
    
  oper mkActivePresParticipleTable : Str * ThVowelPres -> AdjTable;
  oper mkActivePresParticipleTable info = 
    case info of {
      <stem,PR_I> => record2table { 
        msnom = stem + "įs" ; -- intis
        msacc = stem + "intį" ;
        msgen = stem + "inčio" ;
        msins = stem + "inčiu" ;
        msdat = stem + "inčiam" ;
        msloc = stem + "inčiame" ;

        mpnom = stem + "į" ; -- intys
        mpacc = stem + "inčius" ;
        mpgen = stem + "inčių" ;
        mpins = stem + "inčiais" ;
        mpdat = stem + "inčiems" ;
        mploc = stem + "inčiuose" ;

        fsnom = stem + "inti" ;
        fsacc = stem + "inčią" ;
        fsgen = stem + "inčios" ;
        fsins = stem + "inčia" ;
        fsdat = stem + "inčiai" ;
        fsloc = stem + "inčioje" ;

        fpnom = stem + "inčios" ;
        fpacc = stem + "inčias" ;
        fpgen = stem + "inčių" ;
        fpins = stem + "inčiomis" ;
        fpdat = stem + "inčioms" ;
        fploc = stem + "inčiose" ;

        nnom = stem + "į" ; 
        } ;
      <stem,PR_IA> => record2table { 
        msnom = stem + "iąs" ; -- iantis
        msacc = stem + "iantį" ;
        msgen = stem + "iančio" ;
        msins = stem + "iančiu" ;
        msdat = stem + "iančiam" ;
        msloc = stem + "iančiame" ;

        mpnom = stem + "ią" ; -- iantys
        mpacc = stem + "iančius" ;
        mpgen = stem + "iančių" ;
        mpins = stem + "iančiais" ;
        mpdat = stem + "iančiems" ;
        mploc = stem + "iančiuose" ;

        fsnom = stem + "ianti" ;
        fsacc = stem + "iančią" ;
        fsgen = stem + "iančios" ;
        fsins = stem + "iančia" ;
        fsdat = stem + "iančiai" ;
        fsloc = stem + "iančioje" ;

        fpnom = stem + "iančios" ;
        fpacc = stem + "iančias" ;
        fpgen = stem + "iančių" ;
        fpins = stem + "iančiomis" ;
        fpdat = stem + "iančioms" ;
        fploc = stem + "iančiose" ;

        nnom = stem + "ią" ; 
     } ;
      <stem,_> => record2table { 
        msnom = stem + "ąs" ; -- antis
        msacc = stem + "antį" ;
        msgen = stem + "ančio" ;
        msins = stem + "ančiu" ;
        msdat = stem + "ančiam" ;
        msloc = stem + "ančiame" ;

        mpnom = stem + "ą" ; -- antys
        mpacc = stem + "ančius" ;
        mpgen = stem + "ančių" ;
        mpins = stem + "ančiais" ;
        mpdat = stem + "ančiems" ;
        mploc = stem + "ančiuose" ;

        fsnom = stem + "anti" ;
        fsacc = stem + "ančią" ;
        fsgen = stem + "ančios" ;
        fsins = stem + "ančia" ;
        fsdat = stem + "ančiai" ;
        fsloc = stem + "ančioje" ;

        fpnom = stem + "ančios" ;
        fpacc = stem + "ančias" ;
        fpgen = stem + "ančių" ;
        fpins = stem + "ančiomis" ;
        fpdat = stem + "ančioms" ;
        fploc = stem + "ančiose" ;

        nnom = stem + "ą" ; 
     }
   } ;   
  oper mkVerbTables : ReflStatus -> Str -> Str -> Str -> ConjClass -> Fronting => VForm => Str;
  oper mkVerbTables refl infStem presStem pastStem conjClass = 
      case refl of {
         Postfix => mkFinalReflVerbTables infStem presStem pastStem conjClass ;
         _ => mkUniqueVerbTables infStem presStem pastStem conjClass
      } ;

  mkUniqueVerbTables : Str -> Str -> Str -> ConjClass -> Fronting => VForm =>Str;
  mkUniqueVerbTables infStem presStem pastStem conjClass = 
      let pres = makeVerbPres "" presStem conjClass in
      let negPres = makeVerbPres "ne" presStem conjClass in
      let past = makeVerbPast "" pastStem conjClass in
      let negPast = makeVerbPast "ne" pastStem conjClass in
      let pastFreq = makeVerbPastFreq "" infStem in
      let negPastFreq = makeVerbPastFreq "ne" infStem in
      let fut = makeVerbFut "" infStem in
      let negFut = makeVerbFut "ne" infStem in
      let cond = makeVerbCond "" infStem in
      let negCond = makeVerbCond "ne" infStem in
      let imper = makeVerbImper "" infStem in
      let negImper = makeVerbImper "ne" infStem in
      let gerund = makeGerund "" infStem in
      let negGerund = makeGerund "nesi" infStem in
      table {
        Unfronted => makeVerbTable (infStem + "ti") pres past pastFreq fut cond imper gerund ;
        NePref   => makeVerbTable ("ne" + infStem + "ti") negPres negPast negPastFreq negFut negCond negImper negGerund
      } ;
      
  mkFinalReflVerbTables : Str -> Str -> Str -> ConjClass -> Fronting => VForm =>Str;
  mkFinalReflVerbTables infStem presStem pastStem conjClass = 
      let pres = makeVerbPresRefl presStem conjClass in
      let negPres = makeVerbPres "nesi" presStem conjClass in
      let past = makeVerbPastRefl pastStem conjClass in
      let negPast = makeVerbPast "nesi" pastStem conjClass in
      let pastFreq = makeVerbPastFreqRefl infStem in
      let negPastFreq = makeVerbPastFreq "nesi" infStem in
      let fut = makeVerbFutRefl infStem in
      let negFut = makeVerbFut "nesi" infStem in
      let cond = makeVerbCondRefl infStem in
      let negCond = makeVerbCond "nesi" infStem in
      let imper = makeVerbImperRefl infStem in
      let negImper = makeVerbImper "nesi" infStem in
      let gerund = makeGerundRefl infStem in
      let negGerund = makeGerund "nesi" infStem in
      table {
        Unfronted => makeVerbTable (infStem + "tis") pres past pastFreq fut cond imper gerund ;
        NePref   => makeVerbTable ("nesi" + infStem + "ti") negPres negPast negPastFreq negFut negCond negImper negGerund
      } ;

  mkCopulaVerbTables : Fronting => VForm =>Str;
  mkCopulaVerbTables = 
      let pres = { prsg1 = "esu" ; prsg2 = "esi" ; pr3 = "" ; prpl1 = "esame" ; prpl2 = "esate"} in
      let negPres = { prsg1, prsg2, pr3, prpl1, prpl2 = "ne"} in
      let past = makeVerbPast "" "buv" C1a in
      let negPast = makeVerbPast "ne" "buv" C1a in
      let pastFreq = makeVerbPastFreq "" "bū" in
      let negPastFreq = makeVerbPastFreq "ne" "bū" in
      let fut = makeVerbFut "" "bū" in
      let negFut = makeVerbFut "ne" "bū" in
      let cond = makeVerbCond "" "bū" in
      let negCond = makeVerbCond "ne" "bū" in
      let imper = makeVerbImper "" "bū" in
      let negImper = makeVerbImper "ne" "bū" in
      let gerund = makeGerund "" "bū" in
      let negGerund = makeGerund "nesi" "bū" in
      table {
        Unfronted => makeVerbTable ("bū" + "ti") pres past pastFreq fut cond imper gerund ;
        NePref   => makeVerbTable ("ne" + "bū" + "ti") negPres negPast negPastFreq negFut negCond negImper negGerund
      } ;

  makeVerbTable : Str -> PresForms -> PastForms -> PastFreqForms -> FutForms -> HypForms -> ImperForms -> GerundForms -> VForm => Str;      
  makeVerbTable infForm pres past pastFreq fut cond imper gerund  =      
      table {
          VInf => infForm;

          VImperSg2 => imper.isg2;
          VImperPl1 => imper.ipl1;
          VImperPl2 => imper.ipl2;

          VPres Sg P1 => pres.prsg1 ;
          VPres Sg P2 => pres.prsg2 ;
          VPres Sg P3 => pres.pr3 ;
          VPres Pl P1 => pres.prpl1 ;
          VPres Pl P2 => pres.prpl2 ;
          VPres Pl P3 => pres.pr3 ;

          VPast Sg P1 => past.psg1 ;
          VPast Sg P2 => past.psg2 ;
          VPast Sg P3 => past.p3 ;
          VPast Pl P1 => past.ppl1 ;
          VPast Pl P2 => past.ppl2 ;
          VPast Pl P3 => past.p3 ;

          VPastFreq Sg P1 => pastFreq.pfsg1 ;
          VPastFreq Sg P2 => pastFreq.pfsg2 ;
          VPastFreq Sg P3 => pastFreq.pf3 ;
          VPastFreq Pl P1 => pastFreq.pfpl1 ;
          VPastFreq Pl P2 => pastFreq.pfpl2 ;
          VPastFreq Pl P3 => pastFreq.pf3 ;

          VFut Sg P1 => fut.fsg1 ;
          VFut Sg P2 => fut.fsg2 ;
          VFut Sg P3 => fut.f3 ;
          VFut Pl P1 => fut.fpl1 ;
          VFut Pl P2 => fut.fpl2 ;
          VFut Pl P3 => fut.f3 ;

          VHyp Sg P1 => cond.csg1 ;
          VHyp Sg P2 => cond.csg2 ;
          VHyp Sg P3 => cond.c3 ;
          VHyp Pl P1 => cond.cpl1 ;
          VHyp Pl P2 => cond.cpl2 ;
          VHyp Pl P3 => cond.c3 ;

          VGerund Masc Sg => gerund.ms ;
          VGerund Fem Sg => gerund.fs ;
          VGerund Masc Pl => gerund.mp ;
          VGerund Fem Pl => gerund.fp 
   };



  getInfStem : Str -> Str
    = \v ->
      case v of {
        s + "ti" => s ;
        s + "tis" => s ;
        _ => Predef.error ("Error: incorrect Inf:" + v)
      } ;

  getPresCore : Str -> Str * ThVowelPres
    = \v ->
      case v of {
        s + ("ia" | "iasi") => <s, PR_IA> ;
        s + ("a" | "asi") => <s, PR_A> ;
        s + ("o" | "osi") => <s, PR_O> ;
        s + ("isi" | "i") => <s, PR_I> ;
        _ => Predef.error ("Error: incorrect Pres P3:" + v)
      } ;

  getPastCore : Str -> Str * ThVowelPast
    = \v ->
      case v of {
        s + ("o" | "osi") => <s, P_O> ;
        s + ("ė" | "ėsi") => <s, P_E> ;
        _ => Predef.error ("Error: incorrect Past P3:" + v)
      } ;

-- Passing the stem for the error message could help debugging...
  getConjClass : ThVowelPres -> ThVowelPast -> ConjClass
    = \presThV,pastThV ->
      case <presThV,pastThV> of {
        <PR_A, P_O> => C1a ;
        <PR_A, _> => C1b ;
        <PR_IA, P_E> => C1c ;
        <PR_IA, _> => C1d ;
        <PR_I, P_O> => C2a ;
        <PR_O, P_E> => C3a ;
        <PR_O, _> => C3b ;
         _ => Predef.error ("Error: guessing verb conjugation does not work for the Thematic vowel combintation")
      } ;

  makeVerbPres : Str -> Str -> ConjClass -> PresForms
  -- pfx = ne, nesi (and could be tebe, tebesi and so on)
    = \pfx, presStem, cc ->
       case presStem of {
         "yr" => {
            prsg1 = (case pfx of { "ne" => "nesu"; _ => "esu" });
            prsg2 = pfx + "esi" ;
            pr3   = pfx + "yra" ;
            prpl1 = pfx + "esame" ;
            prpl2 = pfx + "esate" ;
         } ;
         _ => case cc of {
            (C1a|C1b) => {
              prsg1 = pfx + presStem + "u" ;
              prsg2 = pfx + presStem + "i" ;
              pr3   = pfx + presStem + "a" ;
              prpl1 = pfx + presStem + "ame" ;
              prpl2 = pfx + presStem + "ate" ;
            } ;
            (C1c|C1d) => {
            -- kvieč(ia)
              prsg1 = pfx + presStem + "iu" ;
              prsg2 = pfx + (harden presStem) + "i" ;
              pr3   = pfx + presStem + "ia" ;
              prpl1 = pfx + presStem + "iame" ;
              prpl2 = pfx + presStem + "iate" ;
            } ;
            C2a => {
            -- gird(i)
              prsg1 = pfx + (soften presStem) + "iu" ;
              prsg2 = pfx + presStem + "i" ;
              pr3   = pfx + presStem + "i" ;
              prpl1 = pfx + presStem + "ime" ;
              prpl2 = pfx + presStem + "ite" ;
            } ;
            (C3a|C3b) => {
              prsg1 = pfx + presStem + "au" ;
              prsg2 = pfx + presStem + "ai" ;
              pr3   = pfx + presStem + "o" ;
              prpl1 = pfx + presStem + "ome" ;
              prpl2 = pfx + presStem + "ote" ;
            }
         }
       } ;
  
  makeVerbPresRefl : Str -> ConjClass -> PresForms
    = \presStem, cc ->
       case cc of {
          (C1a|C1b) => {
            prsg1 = presStem + "uosi" ;
            prsg2 = presStem + "iesi" ;
            pr3   = presStem + "asi" ;
            prpl1 = presStem + "amės" ;
            prpl2 = presStem + "atės" ;
          } ;
          (C1c|C1d) => {
          -- kvieč(ia)
            prsg1 = presStem + "iuosi" ;
            prsg2 = (harden presStem) + "iesi" ;
            pr3   = presStem + "iasi" ;
            prpl1 = presStem + "iamės" ;
            prpl2 = presStem + "iatės" ;
          } ;
          C2a => {
          -- gird(i)
            prsg1 = (soften presStem) + "iuosi" ;
            prsg2 = presStem + "iesi" ;
            pr3   = presStem + "isi" ;
            prpl1 = presStem + "imės" ;
            prpl2 = presStem + "itės" ;
          } ;
          (C3a|C3b) => {
            prsg1 = presStem + "ausi" ;
            prsg2 = presStem + "aisi" ;
            pr3   = presStem + "osi" ;
            prpl1 = presStem + "omės" ;
            prpl2 = presStem + "otės" ;
          }
       };
  
  makeVerbPast : Str -> Str -> ConjClass -> PastForms
    = \pfx, pastStem, cc ->
    case cc of {
       (C1a|C1d|C2a|C3b) => {
         psg1 = pfx + pastStem + "au" ;
         psg2 = pfx + pastStem + "ai" ;
         p3   = pfx + pastStem + "o" ;
         ppl1 = pfx + pastStem + "ome" ;
         ppl2 = pfx + pastStem + "ote" ;
       } ;
       _ => {
            -- kviet(ė)
         psg1 = pfx + (soften pastStem) + "iau" ;
         psg2 = pfx + pastStem + "ei" ;
         p3   = pfx + pastStem + "ė" ;
         ppl1 = pfx + pastStem + "ėme" ;
         ppl2 = pfx + pastStem + "ėte" ;
       }
    } ;

  makeVerbPastRefl : Str -> ConjClass -> PastForms
    = \pastStem, cc ->
    case cc of {
       (C1a|C1d|C2a|C3b) => {
         psg1 = pastStem + "ausi" ;
         psg2 = pastStem + "aisi" ;
         p3   = pastStem + "osi" ;
         ppl1 = pastStem + "omės" ;
         ppl2 = pastStem + "otės" ;
       } ;
       _ => {
            -- kviet(ė)
         psg1 = (soften pastStem) + "iausi" ;
         psg2 = pastStem + "eisi" ;
         p3   = pastStem + "ėsi" ;
         ppl1 = pastStem + "ėmės" ;
         ppl2 = pastStem + "ėtės" ;
       }
    } ;


  makeVerbPastFreq : Str -> Str -> PastFreqForms
    = \pfx, infStem -> 
      {
         pfsg1 = pfx + infStem + "davau" ;
         pfsg2 = pfx + infStem + "davai" ;
         pf3   = pfx + infStem + "davo" ;
         pfpl1 = pfx + infStem + "davome" ;
         pfpl2 = pfx + infStem + "davote" ;
      } ;

  makeVerbPastFreqRefl : Str -> PastFreqForms
    = \infStem ->
      {
         pfsg1 = infStem + "davausi" ;
         pfsg2 = infStem + "davaisi" ;
         pf3   = infStem + "davosi" ;
         pfpl1 = infStem + "davomės" ;
         pfpl2 = infStem + "davotės" ;
      } ;

-- !!! 3rd p. Shortening
  makeVerbFut : Str -> Str -> FutForms
    = \pfx, infStem -> case infStem of {
      shortStem + "s" =>
       {
         fsg1 = pfx + infStem + "iu" ;
         fsg2 = pfx + infStem + "i" ;
-- !!! 3rd p. Shortening
         f3  = pfx + infStem ;
         fpl1 = pfx + infStem + "ime" ;
         fpl2 = pfx + infStem + "ite" ;
        } ;
      _ =>
       {
         fsg1 = pfx + infStem + "siu" ;
         fsg2 = pfx + infStem + "si" ;
-- !!! 3rd p. Shortening
         f3   = pfx + infStem + "s" ;
         fpl1 = pfx + infStem + "sime" ;
         fpl2 = pfx + infStem + "site" ;
        } 
   } ;

-- !!! 3rd p. Shortening
  makeVerbFutRefl : Str -> FutForms
    = \infStem -> case infStem of {
      _ + "s" =>
       {
         fsg1 = infStem + "iuosi" ;
         fsg2 = infStem + "iesi" ;
-- !!! 3rd p. Shortening
         f3   = infStem + "is" ;
         fpl1 = infStem + "imės" ;
         fpl2 = infStem + "itės" ;
       } ;
      _ =>
       {
         fsg1 = infStem + "siuosi" ;
         fsg2 = infStem + "siesi" ;
-- !!! 3rd p. Shortening
         f3   = infStem + "sis" ;
         fpl1 = infStem + "simės" ;
         fpl2 = infStem + "sitės" ;
       } 
   } ;

  makeVerbCond : Str -> Str -> HypForms
    = \pfx, infStem ->
     {
         csg1 = pfx + infStem + "čiau" ;
         csg2 = pfx + infStem + "tum" ;
         c3   = pfx + infStem + "tų" ;
         cpl1 = pfx + infStem + "tume" ;
         cpl2 = pfx + infStem + "tumėte" ;
      } ;

  makeVerbCondRefl : Str -> HypForms
    = \infStem ->
     {
         csg1 = infStem + "čiausi" ;
         csg2 = infStem + "tumeisi" ;
         c3   = infStem + "tųsi" ;
         cpl1 = infStem + "tumės" ;
         cpl2 = infStem + "tumėtės" ;
      } ;

  makeVerbImper : Str -> Str -> ImperForms
    = \pfx, infStem -> case infStem of {
      shortStem + ("g" | "k") =>
       {
           isg2 = pfx + shortStem + "k" ;
           ipl1 = pfx + shortStem + "kime" ;
           ipl2 = pfx + shortStem + "kite" ;
        } ;
      _ =>
       {
           isg2 = pfx + infStem + "k" ;
           ipl1 = pfx + infStem + "kime" ;
           ipl2 = pfx + infStem + "kite" ;
        } 
   } ;

  makeVerbImperRefl : Str -> ImperForms
    = \infStem -> case infStem of {
      shortStem + ("g" | "k") =>
       {
           isg2 = shortStem + "kis" ;
           ipl1 = shortStem + "kimės" ;
           ipl2 = shortStem + "kitės" ;
        } ;
      _ =>
       {
           isg2 = infStem + "kis" ;
           ipl1 = infStem + "kimės" ;
           ipl2 = infStem + "kitės" ;
        } 
   } ;

  makeGerund : Str -> Str -> GerundForms
    = \pfx, infStem ->
     {
         ms = pfx + infStem + "damas" ;
         fs = pfx + infStem + "dama" ;
         mp = pfx + infStem + "dami" ;
         fp = pfx + infStem + "damos" ;
      } ;

  makeGerundRefl : Str -> GerundForms
    = \infStem ->
     {
         ms = infStem + "damasi" ;
         fs = infStem + "damasi" ;
         mp = infStem + "damiesi" ;
         fp = infStem + "damosi" ;
      } ;

-- No PastFreq...
    indicativeForm : Verb -> Polarity -> Tense * Anteriority * GenNum * Person => Str;
    indicativeForm verb pol = 
--      case pol of {
--        <_, Anter, _, _> => mkFormWithCopula verb pol;
--        _ => 
          let forms = verb.forms; in
          let buti = (mkVerb "būti" "yra" "buvo").forms in
          case pol of {
            Pos =>
              table {
                <Pres, Simul, gn, p> => forms ! Unfronted ! (VPres (extract_num!gn) p);
                <Past, Simul, gn, p> => forms ! Unfronted ! (VPast (extract_num!gn) p);
                <Fut,  Simul, gn, p> => forms ! Unfronted ! (VFut (extract_num!gn) p);
                <Cond, Simul, gn, p> => forms ! Unfronted ! (VHyp (extract_num!gn) p) ;
                <Pres, Anter, gn, p> => buti ! Unfronted ! (VPres (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>);
                <Past, Anter, gn, p> => buti ! Unfronted ! (VPast (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>);
                <Fut, Anter, gn, p> => buti ! Unfronted ! (VFut (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>);
                <Cond, Anter, gn, p> => buti ! Unfronted ! (VHyp (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>)
            } ;
            Neg =>
              table {
                <Pres, Simul, gn, p> => forms ! NePref ! (VPres (extract_num!gn) p);
                <Past, Simul, gn, p> => forms ! NePref ! (VPast (extract_num!gn) p);
                <Fut,  Simul, gn, p> => forms ! NePref ! (VFut (extract_num!gn) p);
                <Cond, Simul, gn, p> => forms ! NePref ! (VHyp (extract_num!gn) p);
                <Pres, Anter, gn, p> => "ne" + buti ! Unfronted ! (VPres (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>);
                <Past, Anter, gn, p> => "ne" + buti ! Unfronted ! (VPast (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>);
                <Fut,  Anter, gn, p> => "ne" + buti ! Unfronted ! (VFut (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>);
                <Cond, Anter, gn, p> => "ne" + buti ! Unfronted ! (VHyp (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>)
          }
--        }
     };
        
-- No PastFreq...
    mkFormWithCopula : Verb -> Polarity -> Tense * Anteriority * GenNum * Person => Str;
    mkFormWithCopula verb pol =
        let buti = (mkVerb "būti" "yra" "buvo").forms in
        case pol of {
           Pos =>
             table {
               <Pres, _, gn, p> => buti ! Unfronted ! (VPres (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>);
               <Past, _, gn, p> => buti ! Unfronted ! (VPast (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>);
               <Fut,  _, gn, p> => buti ! Unfronted ! (VFut (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>);
               <Cond, _, gn, p> => buti ! Unfronted ! (VHyp (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>) 
             } ;
           Neg =>
             table {
               <Pres, _, gn, p> => "ne" + buti ! NePref ! (VPres (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>);
               <Past, _, gn, p> => "ne" + buti ! NePref ! (VPast (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>);
               <Fut,  _, gn, p> => "ne" + buti ! NePref ! (VFut (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>);
               <Cond, _, gn, p> => "ne" + buti ! NePref ! (VHyp (extract_num!gn) p) ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>)
             }
        };

    imperativeForm : Verb -> Polarity -> GenNum -> Person -> Str;
    imperativeForm verb pol gn p = 
--        case pol of {
--            <_, Anter, _, _> =>
--                case pol of {
--                    Pos => bukOp!<(extract_num!gn), p> ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>);
--                    Neg => nebukOp!<(extract_num!gn), p> ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>)
--                };
--            _ => 
                -- a suppr
                case pol of {
                    Pos => case <(extract_num!gn), p> of {
                        <Sg, P1> => "kad" ++ verb.forms ! Unfronted ! VHyp Sg P1;
                        <Sg, P2> => verb.forms ! Unfronted ! VImperSg2;
                        <Sg, P3> => "tegul" ++ verb.forms ! Unfronted ! VPres Sg P3;
                        <Pl, P1> => verb.forms ! Unfronted ! VImperPl1;
                        <Pl, P2> => verb.forms ! Unfronted ! VImperPl2;
                        <Pl, P3> => "tegul" ++ verb.forms ! Unfronted ! VPres Pl P3
                    } ;
                    Neg => case <(extract_num!gn), p> of {
                        <Sg, P1> => "kad" ++ verb.forms ! NePref ! VHyp Sg P1;
                        <Sg, P2> => verb.forms ! NePref ! VImperSg2;
                        <Sg, P3> => "tegul" ++ verb.forms ! NePref ! VPres Sg P3;
                        <Pl, P1> => verb.forms ! NePref ! VImperPl1;
                        <Pl, P2> => verb.forms ! NePref ! VImperPl2;
                        <Pl, P3> => "tegul" ++ verb.forms ! NePref ! VPres Pl P3
                    }
--                }
        };
        
    infinitiveForm : Verb -> Polarity -> GenNum -> Str;
    infinitiveForm verb pol gn = --{
 --           <_, Anter, _, _> =>
 --               case pol of {
 --                   Pos => "būti" ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>);
 --                   Neg => "nebūti" ++ (mkAtable (table2record verb.actPastPart))!(cast_aform!<gn,Nom>)
 --                   };
 --           _ => 
                case pol of {
                    Pos => verb.forms ! Unfronted ! VInf;
                    Neg => verb.forms ! NePref ! VInf
--                }
        };

-- tegu ar tegul ar te
    bukOp : Number * Person => Str = table {
        <Sg, P1> => ["kad būčiau"];
        <Sg, P2> => ["būk"];
        <Sg, P3> => ["tegul būtų"];
        <Pl, P1> => ["būkime"];
        <Pl, P2> => ["būkite"];
        <Pl, P3> => ["tegul būtų"]
    };

    nebukOp : Number * Person => Str = table {
        <Sg, P1> => ["kad nebūčiau"];
        <Sg, P2> => ["nebūk"];
        <Sg, P3> => ["tegul nebūtų"];
        <Pl, P1> => ["nebūkime"];
        <Pl, P2> => ["nebūkite"];
        <Pl, P3> => ["tegul nebūtų"]
    };

-- PastFreq
    yraOp : GenNum * Person * Tense * Anteriority => Str = 
      let buti = (mkVerb "būti" "yra" "buvo").forms in table {
        <gn, p, Pres, _    > => buti ! Unfronted ! (VPres (extract_num!gn) p);
        <gn, p, Past, _    > => buti ! Unfronted ! (VPast (extract_num!gn) p);
        <gn, p, Fut , _    > => buti ! Unfronted ! (VFut (extract_num!gn) p);
        <gn, p, Cond, _    > => buti ! Unfronted ! (VHyp (extract_num!gn) p)
      };

    neraOp : Tense * Anteriority => Str = 
      table {
        <Pres, _> => ["nėra"];
        <Past, _> => ["nebuvo"];
        <Fut , _> => ["nebus"];
        <Cond, _> => ["nebūtų"]
      };

}
