--# -path=.:../abstract:../common:../../prelude

--1 Telugu auxiliary operations.

-- This module contains operations that are needed to make the
-- resource syntax work.

resource ResTel = ParamX ** open Prelude in {

  flags optimize=all ;

  param
    Case = Dir | Obl ;
    Gender = Masc | Fem | Neutr ;

  oper
    Noun = {s : Number => Case => Str ; g : Gender} ;

    mkNoun : (x1,_,_,x4 : Str) -> Gender -> Noun =
      \sd,so,pd,po,g -> {
      s = table Number [table Case [sd;so] ; table Case [pd;po]] ;
      g = g
      } ;

    wallNoun : Str -> Noun = \goda ->
      mkNoun goda goda (goda + "లు") (goda + "ల") Neutr ;

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
     | VImpf Gender Number
     | VPerf Gender Number
     | VSubj Number Person
     | VFut  Number Person Gender
     | VAbs
     | VReq
     | VImp
     | VReqFut
     ;

  oper
    Verb = {s : VForm => Str} ;

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
          VFut  Sg   P1 g => ss1 + ga Sg g ;
          VFut  Sg   _  g => ss2 + ga Sg g ;
          VFut  Pl   P2 g => sp2 + ga Pl g ;
          VFut  Pl   _  g => sp3 + ga Pl g ;
          VAbs  => stem ;
          VReq  => r ;
          VImp  => sp2 ;
          VReqFut => r
          }
        } ;

    regVerb : Str -> Verb = \cal ->
      mkVerb cal cal cal cal cal cal cal cal cal cal cal cal cal cal cal ;

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
        <P1,Sg> => {s = table {PC Dir => "నేను" ; PC Obl => "నా" ; PObj => "నన్ను" ; PPoss => "నా"}} ;
        <P1,Pl> => {s = table {PC Dir => "మేము" ; PC Obl => "మా" ; PObj => "మమ్మల్ని" ; PPoss => "మా"}} ;
        <P2,Sg> => {s = table {PC Dir => "నువ్వు" ; PC Obl => "నీ" ; PObj => "నిన్ను" ; PPoss => "నీ"}} ;
        <P2,Pl> => {s = table {PC Dir => "మీరు" ; PC Obl => "మీ" ; PObj => "మిమ్మల్ని" ; PPoss => "మీ"}} ;
        <P3,Sg> => {s = table {PC Dir => "అతను" ; PC Obl => "అతని" ; PObj => "అతన్ని" ; PPoss => "అతని"}} ;
        <P3,Pl> => {s = table {PC Dir => "వారు" ; PC Obl => "వారి" ; PObj => "వారిని" ; PPoss => "వారి"}}
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
       VPTense VPHTense Agr -- 9 * 12
     | VPReq
     | VPImp
     | VPReqFut
     | VPInf
     | VPStem
     ;

    VType = VIntrans | VTrans | VTransPost ;

  oper
    objVType : VType -> NPCase = \vt -> case vt of {
      VTrans => NPObj ;
      _ => NPC Obl
      } ;

    VPH : Type = {
      s    : Polarity => VPHForm => {fin, inf, neg : Str} ;
      obj  : {s : Str ; a : Agr} ;
      subj : VType ;
      comp : Agr => Str
      } ;

    predV : Verb -> VPH = \verb -> {
      s = \\b,vh =>
       let
         na       = case b of {Pos => []; Neg => "వద్దు" } ;
         negative = case b of {Pos => []; Neg => "లేదు"} ;
       in
       case vh of {
         VPTense VPGenPres (Ag g n p) =>
           {fin = copula CPresent n p g ; inf = verb.s ! VImpf g n ; neg = negative} ;
         VPTense VPImpPast (Ag g n p) =>
           {fin = copula CPast n p g ; inf = verb.s ! VImpf g n ; neg = negative} ;
         VPTense VPContPres (Ag g n p) =>
           {fin = copula CPresent n p g ;
            inf = verb.s ! VStem ++ progressive g n ; neg = negative} ;
         VPTense VPContPast (Ag g n p) =>
           {fin = copula CPast n p g ;
            inf = verb.s ! VStem ++ progressive g n ; neg = negative} ;
         VPTense VPPerf (Ag g n _) =>
           {fin = verb.s ! VPerf g n ; inf = [] ; neg = negative} ;
         VPTense VPPerfPres (Ag g n p) =>
           {fin = copula CPresent n p g ; inf = verb.s ! VPerf g n ; neg = negative} ;
         VPTense VPPerfPast (Ag g n p) =>
           {fin = copula CPast n p g ; inf = verb.s ! VPerf g n ; neg = negative} ;
         VPTense VPSubj (Ag _ n p) => {fin = verb.s ! VSubj n p ; inf = [] ; neg = na} ;
         VPTense VPFut (Ag g n p) => {fin = verb.s ! VFut n p g ; inf = [] ; neg = na} ;
         VPInf => {fin = verb.s ! VStem ; inf = [] ; neg = na} ;
         _ => {fin = verb.s ! VStem ; inf = [] ; neg = na} ----
         } ;
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
      NPObj => Obl ;
      NPErg => Obl
      } ;

    np2pronCase : NPCase -> PronCase = \np -> case np of {
      NPC c => PC c ;
      NPObj => PObj ;
      NPErg => PC Obl
      } ;

    toNP : (Case => Str) -> NPCase -> Str = \pn, npc -> case npc of {
      NPC c => pn ! c ;
      NPObj => pn ! Obl ;
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
