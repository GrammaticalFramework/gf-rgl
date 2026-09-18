--# -path=.:../abstract:../common:../../prelude

--1 Telugu auxiliary operations.

-- This module contains operations that are needed to make the
-- resource syntax work.

resource ResTel = ParamX ** open Prelude in {

  flags optimize=all ;

  param
    Case = Dir | Obl | Acc ;
    Gender = Masc | Fem | Neutr ;

  oper
    Noun = {s : Number => Case => Str ; g : Gender} ;

    mkNoun : (x1,_,_,x4 : Str) -> Gender -> Noun =
      \sd,so,pd,po,g -> {
      s = table Number [
        table Case [sd;so;accusative sd] ;
        table Case [pd;po;accusative pd]
        ] ;
      g = g
      } ;

    accusative : Str -> Str = \word -> case word of {
      stem + "ం" => stem + "ాన్ని" ;
      stem + "ము" => stem + "ాన్ని" ;
      stem + "లు" => stem + "లను" ;
      stem + "ి" => word + "ని" ;
      stem + "ై" => word + "ని" ;
      stem + "ు" => word + "ను" ;
      _ => word + "ను"
      } ;

    wallNoun : Str -> Noun = \word ->
      case word of {
        stem + "ం"  => mkNoun word word (stem + "ాలు") (stem + "ాల") Neutr ;
        stem + "ము" => mkNoun word word (stem + "ములు") (stem + "ముల") Neutr ;
        stem + "లు" => mkNoun word word word (stem + "ల") Neutr ;
        stem + "ుడు" => mkNoun word word (stem + "ులు") (stem + "ుల") Neutr ;
        stem + "ి"  => mkNoun word word (stem + "ులు") (stem + "ుల") Neutr ;
        _            => mkNoun word word (word + "లు") (word + "ల") Neutr
        } ;

    reggNoun : Str -> Gender -> Noun = \s,g ->
      wallNoun s ** {g = g} ;

    regNoun : Str -> Noun = wallNoun ;


    Adjective = {s : Gender => Number => Case => Str} ;

    mkAdjective : (x1,x2,x3 : Str) -> Adjective = \smd,sm,f -> {
      s = \\g,n,c => case <g,n,c> of {
        <Masc,Sg,Dir> => smd ;
        <Masc>        => sm ;
        _             => f
        }
      } ;

    regAdjective : Str -> Adjective = \s -> mkAdjective s s s ;

  param
    VForm =
       VInf
     | VStem
     | VPresent Gender Number Person
     | VPast Gender Number Person
     | VFuture Gender Number Person
     | VNegFinite Bool Gender Number Person
     | VPresentPart
     | VRelativePresent
     | VPastPart
     | VHortative
     | VImpf Gender Number
     | VPerf Gender Number
     | VSubj Number Person
     | VAbs
     | VImp Number Polarity
     | VReqFut
     ;

  oper
    Verb = {s, passive : VForm => Str} ;

    mkVerb : (x1,_,_,_,_,_,_,_,_,_,_,_,_,_,x15 : Str) -> Verb =
      \inf,stem,ims,imp,ifs,ifp,pms,pmp,pfs,pfp,ss1,ss2,sp2,sp3,r -> {
        s =
        let ga : Number -> Gender -> Str = \_,_ -> []
        in table {
          VInf => inf ;
          VStem => stem ;
          VImpf Masc Sg => ims ;
          VImpf Masc Pl => imp ;
          VImpf Fem  Sg => ifs ;
          VImpf Fem  Pl => ifp ;
          VImpf Neutr Sg => ifs ;
          VImpf Neutr Pl => ifp ;
          VPerf Masc Sg => pms ;
          VPerf Masc Pl => pmp ;
          VPerf Fem  Sg => pfs ;
          VPerf Fem  Pl => pfp ;
          VPerf Neutr Sg => pfs ;
          VPerf Neutr Pl => pfp ;
          VSubj Sg   P1 => ss1 ;
          VSubj Sg   _  => ss2 ;
          VSubj Pl   P2 => sp2 ;
          VSubj Pl   _  => sp3 ;
          VAbs  => stem ;
          VImp Sg Pos => sp2 ;
          VImp Sg Neg => verbRoot stem + "వద్దు" ;
          VImp Pl Pos => r ;
          VImp Pl Neg => verbRoot stem + "కండి" ;
          VReqFut => r ;
          VPresent g n p => presentFinite stem (Ag g n p) ;
          VPast g n p => pastFinite stem (Ag g n p) ;
          VFuture g n p => futureFinite stem (Ag g n p) ;
          VNegFinite future g n p => case future of {
            True => verbRoot stem + negativeEnding (Ag g n p) ;
            False => verbRoot stem + "లేదు"
            } ;
          VPresentPart => nonPastStem stem + "ున్న" ;
          VRelativePresent => relativePresent stem ;
          VPastPart => pastStem stem + "ిన" ;
          VHortative => verbRoot stem + "దాం"
          } ;
        passive = conjugation (verbRoot stem + "బడు")
        } ;

    regVerb : Str -> Verb = \verb -> {
      s = conjugation verb ;
      passive = conjugation (verbRoot verb + "బడు")
      } ;

    conjugation : Str -> VForm => Str = \verb ->
      let root = verbRoot verb in table {
        VInf => root + "డం" ;
        VStem => verb ;
        VPresent g n p => presentFinite verb (Ag g n p) ;
        VPast g n p => pastFinite verb (Ag g n p) ;
        VFuture g n p => futureFinite verb (Ag g n p) ;
        VNegFinite future g n p => case future of {
          True => root + negativeEnding (Ag g n p) ;
          False => root + "లేదు"
          } ;
        VPresentPart => nonPastStem verb + "ున్న" ;
        VRelativePresent => relativePresent verb ;
        VPastPart => pastStem verb + "ిన" ;
        VHortative => root + "దాం" ;
        VImpf g n => nonPastStem verb + "ున్న" ;
        VPerf g n => pastStem verb ;
        VSubj n p => nonPastStem verb + finiteEnding (Ag Masc n p) ;
        VAbs => pastStem verb + "ి" ;
        VImp Sg Pos => verb ;
        VImp Sg Neg => root + "వద్దు" ;
        VImp Pl Pos => root + "ండి" ;
        VImp Pl Neg => root + "కండి" ;
        VReqFut => root + "ండి"
        } ;

    -- The WordNet lexicon gives verbs in the usual dictionary/imperative
    -- form.  These operations provide the productive spoken-Telugu stems.
    -- Irregular high-frequency verbs are listed before the regular suffix
    -- rules; compound verbs are handled because matching is suffix based.
    verbRoot : Str -> Str = \verb -> case verb of {
      stem + "ండి" => stem ;
      stem + "ు" => stem ;
      _ => verb
      } ;

    pastStem : Str -> Str = \verb -> case verb of {
      stem + "చేయు" => stem + "చేశ" ;
      stem + "తిను" => stem + "తిన్న" ;
      stem + "విను" => stem + "విన్న" ;
      stem + "కొను" => stem + "కొన్న" ;
      stem + "కను" => stem + "కన్న" ;
      stem + "ఉండు" => stem + "ఉన్న" ;
      stem + "వచ్చు" => stem + "వచ్చ" ;
      stem + "పో" => stem + "పోయ" ;
      stem + "ను" => stem + "న్న" ;
      stem + "ు" => stem ;
      _ => verb
      } ;

    nonPastStem : Str -> Str = \verb -> case verb of {
      stem + "చేయు" => stem + "చేస్త" ;
      stem + "అవు" => stem + "అవుత" ;
      stem + "వెళ్ళు" => stem + "వెళ్త" ;
      stem + "పో" => stem + "పోత" ;
      stem + "తిను" => stem + "తింట" ;
      stem + "విను" => stem + "వింట" ;
      stem + "కొను" => stem + "కొంట" ;
      stem + "ను" => stem + "ంట" ;
      stem + "ించు" => stem + "ిస్త" ;
      stem + "చు" => stem + "స్త" ;
      stem + "ు" => stem + "ుత" ;
      _ => verb + "త"
      } ;

    relativePresent : Str -> Str = \verb -> case verb of {
      stem + "చేయు" => stem + "చేసే" ;
      stem + "వెళ్ళు" => stem + "వెళ్ళే" ;
      stem + "వచ్చు" => stem + "వచ్చే" ;
      _ => verbRoot verb + "ే"
      } ;

    finiteEnding : Agr -> Str = \agr -> case agr of {
      Ag _     Sg P1 => "ాను" ;
      Ag _     Sg P2 => "ావు" ;
      Ag Masc  Sg P3 => "ాడు" ;
      Ag _     Sg P3 => "ింది" ;
      Ag _     Pl P1 => "ాము" ;
      Ag Neutr Pl P3 => "ాయి" ;
      Ag _     Pl _  => "ారు"
      } ;

    presentFinite : Str -> Agr -> Str = \verb,agr ->
      nonPastStem verb + "ున్న" + case agr of {
        Ag _     Sg P1 => "ాను" ;
        Ag _     Sg P2 => "ావు" ;
        Ag Masc  Sg P3 => "ాడు" ;
        Ag _     Sg P3 => "ది" ;
        Ag _     Pl P1 => "ాము" ;
        Ag Neutr Pl P3 => "ాయి" ;
        Ag _     Pl _  => "ారు"
        } ;

    pastFinite : Str -> Agr -> Str = \verb,agr -> case agr of {
      Ag Masc Sg P3 => pastStem verb + "ాడు" ;
      Ag _ Sg P3 => case verb of {
        stem + "చేయు" => stem + "చేసింది" ;
        stem + "తిను" => stem + "తిన్నది" ;
        stem + "విను" => stem + "విన్నది" ;
        stem + "కొను" => stem + "కొన్నది" ;
        stem + "కను" => stem + "కన్నది" ;
        _ => pastStem verb + case agr of {
          Ag Masc Sg P3 => "ాడు" ;
          _ => "ింది"
          }
        } ;
      _ => pastStem verb + finiteEnding agr
      } ;

    futureFinite : Str -> Agr -> Str = \verb,agr ->
      nonPastStem verb + case agr of {
        Ag _     Sg P1 => "ాను" ;
        Ag _     Sg P2 => "ావు" ;
        Ag Masc  Sg P3 => "ాడు" ;
        Ag _     Sg P3 => "ుంది" ;
        Ag _     Pl P1 => "ాము" ;
        Ag Neutr Pl P3 => "ాయి" ;
        Ag _     Pl _  => "ారు"
        } ;

    negativeEnding : Agr -> Str = \agr -> case agr of {
      Ag _     Sg P1 => "ను" ;
      Ag _     Sg P2 => "వు" ;
      Ag Masc  Sg P3 => "డు" ;
      Ag _     Sg P3 => "దు" ;
      Ag _     Pl P1 => "ము" ;
      Ag Neutr Pl P3 => "వు" ;
      Ag _     Pl _  => "రు"
      } ;

    finitePast : Verb -> Agr -> Str = \verb,agr ->
      case agr of {Ag g n p => verb.s ! VPast g n p} ;

    finitePresent : Verb -> Agr -> Str = \verb,agr ->
      case agr of {Ag g n p => verb.s ! VPresent g n p} ;

    finiteFuture : Verb -> Agr -> Str = \verb,agr ->
      case agr of {Ag g n p => verb.s ! VFuture g n p} ;

    finiteNegative : Verb -> VPHTense -> Agr -> Str = \verb,tense,agr ->
      case agr of {Ag g n p => verb.s ! VNegFinite (case tense of {
        VPFut => True ; _ => False}) g n p} ;

    passiveV : Verb -> Verb = \verb -> {
      s = verb.passive ;
      passive = verb.passive
      } ;

    predCopula : VPH = {
      s = \\pol,form => case form of {
        VPTense tense agr => case pol of {
          Neg => {fin = case tense of {
                    VPGenPres => negativeCopula agr ;
                    _ => "కాలేదు"
                    } ; inf = [] ; neg = []} ;
          Pos => {fin = case tense of {
                    VPGenPres => [] ;
                    VPContPres => [] ;
                    VPPerf => copulaPast agr ;
                    VPPerfPres => copulaPast agr ;
                    VPImpPast => copulaPast agr ;
                    VPContPast => copulaPast agr ;
                    VPPerfPast => copulaPast agr ;
                    VPSubj => futureFinite "అవు" agr ;
                    VPFut => futureFinite "అవు" agr
                    } ; inf = [] ; neg = []}
          } ;
        VPInf => {fin = "అవడం" ; inf = [] ; neg = []} ;
        VPStem => {fin = "అవు" ; inf = [] ; neg = []} ;
        VPPresPart => {fin = "అవుతున్న" ; inf = [] ; neg = []} ;
        VPRelPresent => {fin = "అయ్యే" ; inf = [] ; neg = []} ;
        VPPastPart => {fin = "అయిన" ; inf = [] ; neg = []} ;
        VPHort => {fin = "అవుదాం" ; inf = [] ; neg = []} ;
        VPImp Sg => {fin = "అవు" ;   inf = [] ; neg = []} ;
        VPImp Pl => {fin = "అవండి" ; inf = [] ; neg = []} ;
        VPReqFut => {fin = "అవండి" ; inf = [] ; neg = []}
        } ;
      passive = verbForms (regVerb "అవు") ;
      obj = {s = [] ; a = defaultAgr} ;
      subj = VIntrans ;
      comp = \\_ => []
      } ;

    negativeCopula : Agr -> Str = \agr -> case agr of {
      Ag _ Pl _ => "కారు" ;
      _ => "కాదు"
      } ;

    copulaPast : Agr -> Str = \agr -> case agr of {
      Ag _     Sg P1 => "అయ్యాను" ;
      Ag _     Sg P2 => "అయ్యావు" ;
      Ag Masc  Sg P3 => "అయ్యాడు" ;
      Ag _     Sg P3 => "అయింది" ;
      Ag _     Pl P1 => "అయ్యాము" ;
      Ag Neutr Pl P3 => "అయ్యాయి" ;
      Ag _     Pl _  => "అయ్యారు"
      } ;

  param
    CTense = CPresent | CPast | CFuture ;
  oper
    copula : CTense -> Number -> Person -> Gender -> Str = \t,n,p,g ->
      case <t,n,p,g> of {
        _ => []
        } ;

  param
    PronCase = PC Case | PObj | PPoss ;
  oper
    personalPronoun : Person -> Number -> {s : PronCase => Str} = \p,n ->
      case <p,n> of {
        <P1,Sg> => {s = table {PC Dir => "నేను" ; PC Obl => "నా" ; PC Acc => "నన్ను" ; PObj => "నన్ను" ; PPoss => "నా"}} ;
        <P1,Pl> => {s = table {PC Dir => "మేము" ; PC Obl => "మా" ; PC Acc => "మమ్మల్ని" ; PObj => "మమ్మల్ని" ; PPoss => "మా"}} ;
        <P2,Sg> => {s = table {PC Dir => "నువ్వు" ; PC Obl => "నీ" ; PC Acc => "నిన్ను" ; PObj => "నిన్ను" ; PPoss => "నీ"}} ;
        <P2,Pl> => {s = table {PC Dir => "మీరు" ; PC Obl => "మీ" ; PC Acc => "మిమ్మల్ని" ; PObj => "మిమ్మల్ని" ; PPoss => "మీ"}} ;
        <P3,Sg> => {s = table {PC Dir => "అతను" ; PC Obl => "అతని" ; PC Acc => "అతన్ని" ; PObj => "అతన్ని" ; PPoss => "అతని"}} ;
        <P3,Pl> => {s = table {PC Dir => "వారు" ; PC Obl => "వారి" ; PC Acc => "వారిని" ; PObj => "వారిని" ; PPoss => "వారి"}}
        } ;
      ---- the third is the vocative - is it really this way?

  -- the Telugu verb phrase

---    CTense = CPresent | CPast | CFuture ;



  param
    VPHTense =
       VPGenPres  -- impf hum       nahim    "I go"
     | VPImpPast  -- impf Ta        nahim    "I went"
     | VPContPres -- stem raha hum  nahim    "I am going"
     | VPContPast -- stem raha Ta   nahim    "I was going"
     | VPPerf     -- perf           na/nahim "I went"
     | VPPerfPres -- perf hum       na/nahim "I have gone"
     | VPPerfPast -- perf Ta        na/nahim "I had gone"
     | VPSubj     -- subj           na       "I may go"
     | VPFut      -- fut            na/nahim "I shall go"
     ;

    VPHForm =
       VPInf
     | VPTense VPHTense Agr -- 9 * 12
     | VPImp Number
     | VPReqFut
     | VPStem
     | VPPresPart
     | VPRelPresent
     | VPPastPart
     | VPHort
     ;

    VType = VIntrans | VTrans | VTransPost ;

  oper
    objVType : VType -> NPCase = \vt -> case vt of {
      VTrans => NPObj ;
      _ => NPC Obl
      } ;

    FiniteForm : Type = {fin, inf, neg : Str} ;

    VPH : Type = {
      s, passive : Polarity => VPHForm => FiniteForm ;
      obj  : {s : Str ; a : Agr} ;
      subj : VType ;
      comp : Agr => Str
      } ;

    verbForms : Verb -> (Polarity => VPHForm => FiniteForm) = \verb -> \\b,vh =>
       let
         na       = case b of {Pos => []; Neg => "వద్దు" } ;
         negative = case b of {Pos => []; Neg => "లేదు"} ;
       in
       case vh of {
         VPTense tense agr => case b of {
           Neg => {fin = finiteNegative verb tense agr ; inf = [] ; neg = []} ;
           Pos => case tense of {
             VPGenPres  => {fin = finiteFuture verb agr ; inf = [] ; neg = []} ;
             VPImpPast  => {fin = finitePast verb agr ; inf = [] ; neg = []} ;
             VPContPres => {fin = finitePresent verb agr ; inf = [] ; neg = []} ;
             VPContPast => {fin = finitePast verb agr ; inf = [] ; neg = []} ;
             VPPerf     => {fin = finitePast verb agr ; inf = [] ; neg = []} ;
             VPPerfPres => {fin = finitePast verb agr ; inf = [] ; neg = []} ;
             VPPerfPast => {fin = finitePast verb agr ; inf = [] ; neg = []} ;
             VPSubj     => {fin = finiteFuture verb agr ; inf = [] ; neg = []} ;
             VPFut      => {fin = finiteFuture verb agr ; inf = [] ; neg = []}
             }
           } ;
         {- The finite cases above deliberately precede the legacy form
            table.  The latter remains part of the public paradigms API. -}
         VPInf => {fin = verb.s ! VInf ; inf = [] ; neg = na} ;
         VPStem => {fin = verb.s ! VStem ; inf = [] ; neg = na} ;
         VPPresPart => {fin = verb.s ! VPresentPart ; inf = [] ; neg = na} ;
         VPRelPresent => {fin = verb.s ! VRelativePresent ; inf = [] ; neg = na} ;
         VPPastPart => {fin = verb.s ! VPastPart ; inf = [] ; neg = na} ;
         VPHort => {fin = verb.s ! VHortative ; inf = [] ; neg = na} ;
         VPImp n => {fin = verb.s ! VImp n b ; inf = [] ; neg = []} ;
         VPReqFut => {fin = verb.s ! VReqFut ; inf = [] ; neg = na}
         } ;

    predV : Verb -> VPH = \verb -> {
      s = verbForms verb ;
      passive = verbForms (passiveV verb) ;
      obj = {s = [] ; a = defaultAgr} ;
      subj = VIntrans ;
      comp = \\_ => []
      } ;

    progressive : Gender -> Number -> Str = \_,_ -> [] ;

    VPHSlash = VPH ** {c2 : Compl} ;

    Clause : Type = {s : VPHTense => Polarity => Str} ;

    Compl : Type = {s : Str ; c : VType} ;

    insertObject : NP -> VPHSlash -> VPH = \np,vps -> {
      s = vps.s ;
      passive = vps.passive ;
      obj = {s = vps.obj.s ++ np.s ! objVType vps.c2.c ++ vps.c2.s ; a = np.a} ;
      subj = vps.c2.c ;
      comp = vps.comp
      } ;

    insertAdv : Str -> VPH -> VPH = \adv,vp -> vp ** {
      comp = \\agr => vp.comp ! agr ++ adv
      } ;

    tenseVPH : Tense -> Anteriority -> VPHTense = \tense,ant ->
      case <tense,ant> of {
        <Pres,Simul> => VPGenPres ;
        <Past,Simul> => VPPerf ;
        <Fut, Simul> => VPFut ;
        <Cond,Simul> => VPSubj ;
        <Pres,Anter> => VPPerfPres ;
        <Past,Anter> => VPPerfPast ;
        <Fut, Anter> => VPPerf ;
        <Cond,Anter> => VPPerf
        } ;

    positivePolarity : Polarity -> Bool = \pol -> case pol of {
      Pos => True ;
      Neg => False
      } ;

  param
    Agr = Ag Gender Number Person ;
    NPCase = NPC Case | NPObj | NPErg ;

  oper
    agrP3 : Gender -> Number -> Agr = \g,n -> Ag g n P3 ;

    defaultAgr : Agr = agrP3 Masc Sg ;

    npcase2case : NPCase -> Case = \npc -> case npc of {
      NPC c => c ;
      NPObj => Acc ;
      NPErg => Obl
      } ;

    np2pronCase : NPCase -> PronCase = \np -> case np of {
      NPC c => PC c ;
      NPObj => PObj ;
      NPErg => PC Obl
      } ;

    toNP : (Case => Str) -> NPCase -> Str = \pn, npc -> case npc of {
      NPC c => pn ! c ;
      NPObj => pn ! Acc ;
      NPErg => pn ! Obl
      } ;

    NP : Type = {s : NPCase => Str ; a : Agr} ;

    mkClause : NP -> VPH -> Clause = \np,vp -> {
      s = \\vt,b =>
        let
          subj = NPC Dir ;
          agr  = np.a ;
          vps  = vp.s ! b ! VPTense vt agr ;
        in
        np.s ! subj ++ vp.obj.s ++ vp.comp ! np.a ++ vps.neg ++ vps.inf ++ vps.fin
      } ;


}
