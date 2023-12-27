--# -path=.:../abstract:../common:../prelude:

--1 German auxiliary operations.
--
-- (c) 2002-2006 Aarne Ranta and Harald Hammarström
--
-- This module contains operations that are needed to make the
-- resource syntax work. To define everything that is needed to
-- implement $Test$, it moreover contains some lexical
-- patterns needed for $Lex$.

resource ResGer = ParamX ** open Prelude in {

  flags optimize=all ;
    coding=utf8 ;

--------------------------------------------
--PARAMETERS DEFINITIONS
--------------------------------------------

--2 For $Noun$

-- These are the standard four-value case and three-value gender.

  param
    Case = Nom | Acc | Dat | Gen ;
    Gender = Masc | Fem | Neutr ;

-- Complex $CN$s, like adjectives, have strong and weak forms.

    Adjf = Strong | Weak | Mixed | MixedStrong ;

-- Gender distinctions are only made in the singular. 

    GenNum = GSg Gender | GPl ;
    RelGenNum = RGenNum GenNum | RSentence ;

-- Agreement of $NP$ has three parts: gender, number and person.

    -- These 3*2*3 = 18 values can be reduced to 9, since gender is used only
    -- in 3rd person singular. To select reflexive and possessive forms for "Sie"
    -- in (mkClause str agr vp), add a value AgPlPol, for "man", use AgSgP3 Masc.  HL 29.9.2023

    Agr = AgSgP1 | AgSgP2 | AgSgP3 Gender | AgPl Person | AgPlPol ;

  oper
    genderAgr : Agr -> Gender = \r -> case r of {AgSgP3 g => g ; _ => Masc} ;

    numberAgr = overload {
      numberAgr : Agr -> Number = \r -> case r of {
        AgSgP1 | AgSgP2 | AgSgP3 _ => Sg ;
        AgPl _ | AgPlPol => Pl
      } ;
      numberAgr : VAgr -> Number = \r -> case r of {VAg n _ => n} ;
    } ;
    personAgr = overload {
      personAgr : Agr -> Person = \r -> case r of {
        AgSgP1 | AgPl P1 => P1 ;
        AgSgP2 | AgPl P2 => P2 ;
        AgSgP3 _ | AgPl P3 | AgPlPol => P3
      } ;
      personAgr : VAgr -> Person = \r -> case r of {VAg _ p => p}
      } ;

    conjAgr : Agr -> Agr -> Agr = \a,b ->
      let n : Number = conjNumber (numberAgr a) (numberAgr b) ;
          p : Person = conjPerson (personAgr a) (personAgr b)
      in case <n,p> of {<Pl,p> => AgPl p ;
                        <Sg,P3> => AgSgP3 Neutr ;
                        <Sg,P1> => AgSgP1 ;
                        <Sg,P2> => AgSgP2 } ;

-- Pronouns are the worst-case noun phrases, which have both case
-- and possessive forms.

  param NPForm = NPCase Case | NPPoss GenNum Case ;

-- Possessive pronouns have special forms for stand-alone usage as NP (mein.sp = meiner).

  param PossForm = PossF GenNum Case ;

-- Predeterminers sometimes require a case ("ausser mir"), sometimes not ("nur ich").
-- A number is sometimes inherited ("alle Menschen"), sometimes forced ("jeder von
-- den Menschen").

  param 
    PredetCase = NoCase | PredCase Case ;
    PredetAgr = PAg Number | PAgNone ;
  oper
    noCase : {p : Str ; k : PredetCase} = {p = [] ; k = NoCase} ;

-- Pronominal nps are ordered differently, and light nps come before negation in clauses.
-- (To save space, reduce isPron * isLight = 4 values to the following three.) HL 9/19
  param
    Weight = WPron | WLight | WHeavy | WDefArt ;
  oper
    isPron : {w : Weight} -> Bool = \np ->
      case np.w of {WPron => True ; _ => False} ;
    isLight : {w : Weight} -> Bool = \np ->
      case np.w of {WHeavy => False ; _ => True} ;

--2 For $Adjective$

-- The predicative form of adjectives is not inflected further.

  param AForm = APred | AMod GenNum Case ;  


--2 For $Verb$

  param VAgr =           -- Gender is irrelevant for verb forms, HL 8/2023
    VAg Number Person ;  -- except participles

  param VForm =
     VInf Bool           -- True = with the particle "zu"
   | VFin Bool VFormFin  -- True = prefix glued to verb
   | VImper    Number    -- prefix never glued
   | VPresPart AForm     -- prefix always glued
   | VPastPart AForm ;

  param VFormFin =
     VPresInd  Number Person
   | VPresSubj Number Person
   | VImpfInd  Number Person --# notpresent
   | VImpfSubj Number Person --# notpresent
   ;

  param VPForm =
     VPFinite  Mood Tense Anteriority
   | VPImperat Bool
   | VPInfinit Anteriority ;

  param VAux = VHaben | VSein ;

  param VType = VAct | VRefl Case ;

-- The order of a sentence depends on whether it is used as a main
-- clause, inverted, or subordinate.

  param  
    Order = Main | Inv | Sub ;

-- Main clause mood: "es sei, es wäre, es werde sein".
-- Not relevant for $Fut$. ---

    Mood = MIndic | MConjunct ;

--2 For $Relative$
 
    RAgr = RNoAg | RAg Number Person ;

--2 For $Numeral$

    CardOrd = NCard AForm | NOrd AForm ;
    DForm = DUnit  | DTeen  | DTen ;

--2 Transformations between parameter types

  oper
    agrP3 : Number -> Agr = agrgP3 Neutr ;

    agrgP3 : Gender -> Number -> Agr = \g,n -> case n of {
      Sg => AgSgP3 g ;
      Pl => AgPl P3    -- no gender in Pl
      } ;

    gennum : Gender -> Number -> GenNum = \g,n ->
      case n of {
        Sg => GSg g ;
        Pl => GPl
        } ;

-- Needed in $RelativeGer$ and $NounGer$.

    numGenNum : GenNum -> Number = \gn -> 
      case gn of {
        GSg _ => Sg ;
        GPl   => Pl
        } ;

    genGenNum : GenNum -> Gender = \gn ->
      case gn of {GSg g => g ; GPl => Masc} ;

-- Used in $NounGer$.

    agrAdj : Adjf -> GenNum -> Case -> AForm = \a,gn,c ->
      let
        e  = AMod (GSg Fem) Nom ;
        en = AMod (GSg Masc) Acc ;
      in
      case a of {
        Strong => AMod gn c ;
        Weak => case <gn,c> of {
          <GSg _,   Nom> => e ;
          <GSg Masc,Acc> => en ;
          <GSg _,   Acc> => e ;
          _              => en } ;
        Mixed => case <gn,c> of {
          <GSg g, Nom|Acc> => AMod gn c ;
          _ => en } ;
        MixedStrong => case <gn,c> of {
          <GSg _, Dat|Gen> => en ;
          _                => AMod gn c }
      } ;


-- This is used twice in NounGer.

    adjfCase : Adjf -> Case -> Adjf = \a,c -> case c of {
         Nom|Acc => a ;
         _ => Weak
         } ;      

    vFin : Bool -> Mood -> Tense -> VAgr -> VForm = \b,m,t,a ->
      let
        an : Number = numberAgr a ;
        ap : Person = personAgr a ;
      in
      case <t,m> of {
        <Pres,MIndic>    => VFin b (VPresInd   an ap) ;
        <Pres,MConjunct> => VFin b (VPresSubj  an ap) 
                                                      ;  --# notpresent
        <Past,MIndic>    => VFin b (VImpfInd   an ap) ;  --# notpresent
        <Past,MConjunct> => VFin b (VImpfSubj  an ap) ;  --# notpresent
        _ => VInf False --# notpresent
        } ;

    agr2vagr : Agr -> VAgr = \r -> case r of {
       AgSgP1   => VAg Sg P1 ;
       AgSgP2   => VAg Sg P2 ;
       AgSgP3 g => VAg Sg P3 ;
       AgPl p   => VAg Pl p ;
       AgPlPol  => VAg Pl P3
     } ;

    vagrP3 : Number -> VAgr = \n -> VAg n P3 ;

--------------------------------------------
--TYPE DEFINITIONS + WORST-CASE CONSTRUCTORS
--------------------------------------------

-- For $Lex$.

-- For conciseness and abstraction, we first define a method for
-- generating a case-dependent table from a list of four forms.

  oper
  caselist : (x1,_,_,x4 : Str) -> Case => Str = \n,a,d,g -> 
    table {
      Nom => n ; 
      Acc => a ; 
      Dat => d ; 
      Gen => g
      } ;

-- For each lexical category, here are the worst-case constructors and
-- some practical special cases.
-- More paradigms are given in $ParadigmsGer$.

-- The worst-case constructor for common nouns needs six forms: all plural forms
-- are always the same except for the dative. Actually the six forms are never
-- needed at the same time, but just subsets of them.

  Noun : Type = {
    s : Number => Case => Str ; 
    co : Str ;  -- compound form, first part
    uncap : {   -- compound form, second part, uncapitalized
      s : Number => Case => Str ;  --- just a copy of the normal noun; would be nicer with run-time uncap
      co : Str ;
      } ;
    g : Gender
    } ;

  NP : Type = { -- HL 7/22: Bool = True if DefArt is dropped to combine with prep of type isPrepDefArt
     s : Bool => Case => Str ;
     rc : Str ;  -- die Frage , [rc die ich gestellt habe]
     ext : Str ; -- die Frage , [sc wo sie schläft] ; die Regel , [vp kein Fleisch zu essen] | [s dass ...]
     a : Agr ;
     w : Weight } ; -- light NPs come before negation in simple clauses

  mkN  : (x1,_,_,_,_,x6,x7 : Str) -> Gender -> Noun = 
    \Mann, Mannen, Manne, Mannes, Maenner, Maennern, Mann_, g -> {
     s = table {
       Sg => caselist Mann Mannen Manne Mannes ;
       Pl => caselist Maenner Maenner Maennern Maenner
       } ; 
     co = Mann_ ;
     uncap = 
       let
         mann = toLowerFirst Mann ;
         mannen = toLowerFirst Mannen ;
         manne = toLowerFirst Manne ;
         mannes = toLowerFirst Mannes ;
         maenner = toLowerFirst Maenner ;
         maennern = toLowerFirst Maennern ;
         mann_ = toLowerFirst Mann_ ;
       in {
         s = table {
           Sg => caselist mann mannen manne mannes ;
           Pl => caselist maenner maenner maennern maenner
           } ; 
         co = mann_ ;
         } ;
     g = g
    } ;

  mkCompoundForm : Str -> Str = \s -> case s of {
    _ + "e" => s + "n" ;
    _ + ("ung" | "heit" | "keit") => s + "s" ;
    _ => s  ---- any more predictable cases?
    } ;

-- Adjectives need four forms: two for the positive and one for the other degrees.

  Adjective : Type = {s : Degree => AForm => Str} ;

  mkA : (x1,_,_,x4 : Str) -> Adjective = \gut,gute,besser,best -> 
	let besten = best + "en" in 
    {s = table {
       Posit  => adjForms gut gute ; 
       Compar => adjForms besser besser ; 
       Superl => adjForms besten best -- HL 12/2023: build ("am" ++ besten) via (OrdSuperl a)
       }                              -- to get zweitbeste, am zweitbesten; *zweit am besten
    } ;

-- Verbs need as many as 12 forms, to cover the variations with
-- suffixes "t" and "st". Auxiliaries like "sein" will have to
-- make extra cases even for this.

  Verb : Type = {
    s : VForm => Str ; 
    prefix : Str ;
    particle : Str ; 
    aux : VAux ; 
    vtype : VType
    } ;

  mkV : (x1,_,_,_,_,_,_,_,_,_,_,x12 : Str) -> Str -> VAux -> Verb = 
    \geben,gebe,gibst,gibt,gebt,gib,
     gab,gabst,gaben,gabt,
     gaebe,gegeben,ein,aux ->
    let 
      einb : Bool -> Str -> Str = \b,geb -> 
        if_then_Str b (ein + geb) geb ;
    in
    {s = table {
      VInf False => ein + geben ;
      VInf True  => 
        if_then_Str (isNil ein) ("zu" ++ geben) (ein + "zu" + geben) ;
      VFin b vf => einb b (case vf of { 
       VPresInd Sg P1  => gebe ;
       VPresInd Sg P2  => gibst ;
       VPresInd Sg P3  => gibt ;
       VPresInd Pl P2  => gebt ;
       VPresInd Pl _   => geben ;
       VImpfInd Sg P2  => gabst ;        --# notpresent
       VImpfInd Sg _   => gab ;          --# notpresent
       VImpfInd Pl P2  => gabt ;         --# notpresent
       VImpfInd Pl _   => gaben ;        --# notpresent
       VImpfSubj Sg P2 => gaebe + "st" ; --# notpresent
       VImpfSubj Sg _  => gaebe ;        --# notpresent
       VImpfSubj Pl P2 => gaebe + "t" ;  --# notpresent
       VImpfSubj Pl _  => gaebe + "n" ;  --# notpresent
       VPresSubj Sg P2 => init geben + "st" ;
       VPresSubj Sg _  => init geben ;       
       VPresSubj Pl P2 => init geben + "t" ; 
       VPresSubj Pl _  => geben             
       }) ;
      VImper Sg        => gib ;
      VImper Pl        => gebt ;
      VPresPart a      => ein + (regA (geben + "d")).s ! Posit ! a ;
      VPastPart a      => ein + (regA gegeben).s ! Posit ! a
      } ;
     prefix = ein ;
     particle = [] ;
     aux = aux ;
     vtype = VAct
     } ;

-- To add a prefix (like "ein") to an already existing verb.

  prefixV : Str -> Verb -> Verb = \ein,verb ->
    let
      vs = verb.s ;
      geben = vs ! VInf False ;
      einb : Bool -> Str -> Str = \b,geb -> 
        if_then_Str b (ein + geb) geb ;
    in
    {s = table {
      VInf False => ein + geben ;
      VInf True  => 
        if_then_Str (isNil ein) ("zu" ++ geben) (ein + "zu" + geben) ;
      VFin b vf => einb b (vs ! VFin b vf) ;
      VImper n    => vs ! VImper n ;
      VPresPart a => ein + (regA (geben + "d")).s ! Posit ! a ;
      VPastPart a => ein + vs ! VPastPart a
      } ;
     prefix = ein ;
     particle = verb.particle ;
     aux = verb.aux ;
     vtype = verb.vtype
     } ;


-- These functions cover many regular cases; full coverage inflectional patterns are
-- defined in $MorphoGer$.

  mkN4 : (x1,_,_,x4 : Str) -> Gender -> Noun = \wein,weines,weine,weinen ->
    mkN wein wein wein weines weine weinen (mkCompoundForm wein) ;

  regA : Str -> Adjective = \blau ->
   let blauest : Str = case blau of {
     _ + ("t" | "d" | "s" | "ß" | "sch" | "z" | "au" | "eu") => blau + "est" ;
     _ => blau + "st"
   }
   in
   mkA blau blau (blau + "er") blauest ;

  regDetA : Str -> Adjective = \blau ->
   let
     rblau = regA blau ;
     dblau = adjFormsDet blau blau
   in {
     s = table {
       Posit => dblau ;
       d => rblau.s ! d
       }
     } ;

  regV : Str -> Verb = \legen ->
    let 
      lege  = init legen ;
      leg   = init lege ;
      legt  = leg + "t" ;
      legte = legt + "e"
    in
    mkV 
      legen lege (leg+"st") legt legt leg 
      legte (legte + "st") (legte + "n") (legte + "t")
      legte ("ge" + legt) 
      [] VHaben ;

-- Prepositions indicate the case of their complement noun phrase.

-- There are three types: (i) cases, (ii) pure pre-, post- and circum-positions, 
-- and (iii) prepositions glued with definite article in singular (using s!(GSg g)).

  param
    PrepType = isCase | isPrep | isPrepDefArt ;                  -- HL 7/2022

  oper
    Preposition : Type = {s : GenNum => Str ; s2:Str ; c : Case ; t : PrepType} ;

  isaCase : Preposition -> Bool = \p -> case p.t of {isCase => True ; _ => False} ;
  isaPrep : Preposition -> Bool = \p -> case p.t of {isPrep => True ; _ => False} ;
  isaPrepDefArt : Preposition -> Bool = \p -> case p.t of {isPrepDefArt => True ; _ => False} ;

-- To apply a preposition to a complement.

  appPrep : Preposition -> (Case => Str) -> Str = \prep,arg ->
    prep.s ! GPl ++ arg ! prep.c ++ prep.s2 ;

  appPrepNP : Preposition -> NP -> Str = \prep,np ->
    let
      g : Gender = genderAgr np.a ;
      n : Number = numberAgr np.a ;
      glues = case <prep.t,n> of {<isPrepDefArt,Sg> => True ; _ => False} ;
      nps = np.s ! glues ! prep.c
    in
    case <glues, np.w> of {
        <True, WDefArt> => -- e.g. "zum Hof|zur Tür|zum Fenster herein"
          prep.s ! (GSg g) ++ nps ++ np.ext ++ prep.s2 ++ np.rc ;
         _ => prep.s ! GPl ++ nps ++ np.ext ++ prep.s2 ++ np.rc
         } ;

{- -- Simplify to test the effect on grammar compilation complexity (without SlashV2VNP):
   -- with glues = False: 27096 msec, 3,2M VerbGer.gfo, 854 SentenceGer.gfo
   --     and SlashV2VNP:102597 msec, 16 M VerbGer.gfo, 854 SentenceGer.gfo (good!)
   appPrepNP : Preposition -> NP -> Str = \prep,np ->
     let
       glues = False ;
       nps = np.s ! glues ! prep.c
     in prep.s ! GPl ++ nps ++ np.ext ++ prep.s2 ++ np.rc ;
-}

  bigNP : NP -> Str = \np -> np.ext ++ np.rc ;

-- To build a preposition from just a case.  -- HL 9/19: no longer used in RGL

  noPreposition : Case -> Preposition = \c -> 
    {s = \\_ => [] ; s2 = [] ; c = c ; t = isCase} ;

-- To build a preposition from just a case.  -- HL 9/19: moved to mkPrep in ParadigmsGer

  PrepNom : Preposition = {s = \\_ => [] ; t = isCase ; c = Nom ; s2 = []} ;

  vonDat  : Preposition = {s=table{GPl => "von" ; GSg Fem => "von der"; _ => "vom"};
                           s2=[]; c=Dat; t=isPrepDefArt} ;

-- To build passive: accusative object -> nom subject; others -> same case or prep

  subjPrep : Preposition -> Preposition = \prep ->
    case <prep.c,prep.t> of {
      <Acc,isCase> => prep ** {c = Nom} ;
      _ => prep
    } ;

-- Pronouns and articles
-- Here we define personal and relative pronouns.
-- All personal pronouns, except "ihr", conform to the simple pattern $mkPronPers$.

   mkPronPers : (x1,_,_,_,x5 : Str) -> Gender -> Number -> Person ->
                {s : NPForm => Str ; a : Agr ; sp : PossForm => Str} =
    \ich,mich,mir,meiner,mein,g,n,p -> {
      s = table {
        NPCase c    => caselist ich mich mir meiner ! c ;
        NPPoss gn c => case pronEnding ! gn ! c of {
          "" => mein ;
          s  => case <n,p> of {
              <Pl,P2> => Predef.tk 2 meiner + s ;
              _ => mein + s
              }
            }
          } ;
      a = case <g,n,p> of {
            <_,Pl,p > => AgPl p ;
            <g,Sg,P3> => AgSgP3 g ;
            <_,Sg,P1> => AgSgP1 ;
            <_,Sg,P2> => AgSgP2 -- for "man", "Sie", set in StructuralGer  HL
        } ;
      sp = table {PossF (GSg Masc) Nom => mein + "er" ;             -- HL 12/23
                  PossF (GSg Neutr) (Nom|Acc) => mein + "es" ;
                  PossF gn c  => mein + (pronEnding ! gn ! c)} ;
      } ;

  pronEnding : GenNum => Case => Str = table {
    GSg Masc => caselist ""  "en" "em" "es" ;
    GSg Fem  => caselist "e" "e"  "er" "er" ;
    GSg Neutr => caselist ""  ""  "em" "es" ;
    GPl      => caselist "e"  "e" "en" "er"
    } ;

  artDef : GenNum => Case => Str = table {
    GSg Masc => caselist "der" "den" "dem" "des" ;
    GSg Fem  => caselist "die" "die" "der" "der" ;
    GSg Neutr => caselist "das" "das" "dem" "des" ;
    GPl      => caselist "die" "die" "den" "der"
    } ;

-- This is used when forming determiners that are like adjectives.

  appAdj : Adjective -> Number => Gender => Case => Str = \adj ->
    let
      ad : GenNum -> Case -> Str = \gn,c -> 
        adj.s ! Posit ! AMod gn c
    in
    \\n,g,c => case n of {Sg => ad (GSg g) c ; _  => ad GPl c} ;

-- This auxiliary gives the forms in each degree of adjectives. 

  adjForms : (x1,x2 : Str) -> AForm => Str = \teuer,teur ->
    table {
      APred => teuer ;
      AMod gn c => teur + adjEnding ! gn ! c  -- Strong Adjf
    } ;

  adjEnding : GenNum => Case => Str = table {
    GSg Masc  => caselist "er" "en" "em" "en" ;
    GSg Fem   => caselist "e"  "e"  "er" "er" ;
    GSg Neutr => caselist "es" "es" "em" "en" ;
    GPl       => caselist "e"  "e"  "en" "er"
    } ;

  -- for some determiners, Gen form -es rather than -en
  adjFormsDet : (x1,x2 : Str) -> AForm => Str = \teuer,teur ->
   let adj = adjForms teuer teur
   in
   table {
     AMod (GSg Masc| GSg Neutr) Gen => teur + "es" ;
     a => adj ! a
     } ;

  detEnding : GenNum => Case => Str = table {
    GSg Masc  => caselist "er" "en" "em" "es" ;
    GSg Fem   => caselist "e"  "e"  "er" "er" ;
    GSg Neutr => caselist "es" "es" "em" "es" ;
    GPl       => caselist "e"  "e"  "en" "er"
    } ;

--------------------------------------------
--VP CONSTRUCTION
--------------------------------------------

-- For $Verb$.

  VPC : Type = {
      s : Bool => VAgr => VPForm => { -- True = prefix glued to verb
        fin : Str ;          -- wird
        inf, inf2 : Str      -- lesen,[] | gelesen,haben | können,haben (= gekonnt,haben)
        }                    -- HL 11/6/2019 Fut Anter: lesen gekonnt haben => haben lesen können 
      } ;

   VP : Type = {
      s  : Verb ;                         -- HL 6/2019: <refl|pron,NP,PP,AP|CN|Adv>
      nn : Agr => Str * Str * Str * Str ; --            <sich|ihr,deine Frau,an sie,gut>
      a1 : Str ;              -- was: adV inserted before negation, unused?
      a2 : Str ;              -- adverb
      adj : Str ;             -- adjectival complement of V(2)A, e.g. ich finde dich schön
      isAux : Bool ;          -- is a double infinitive?, e.g. müssen:VV, lassen:V2V
      ext : Str ;             -- sentential complement of V(2)S, V(2)Q, e.g. dass|ob sie kommt
      inf : {inpl: (Agr => Str)*Str ; -- infinitival complement of V(2)V       HL 3/2022
             extr: (Agr => Str)} ;    -- e.g. ihn [] versuchen (lasse) [, ihr zu helfen]
      c1 : Preposition        -- case of subject
     } ;

  VPSlash = VP ** {c2 : Preposition ; objCtrl : Bool} ;  -- HL 3/2019 objCtr added

  -- objCtrl distinguishes object-control from subject-control verb v:V2V in VP.s:
  -- if True, reflexives in vp.inf and vp.nn have to agree with c2-object (added
  -- by ComplSlash), else with subject (added by mkClause).

  useVP : VP -> VPC = \vp ->
    let
      isAux = vp.isAux ;
      verb = vp.s ;
      vfin : Bool -> Mood -> Tense -> VAgr -> Str = \b,m,t,a ->
        verb.s ! vFin b m t a ;
      vinf = verb.s ! VInf False ;
      vpart = if_then_Str isAux vinf (verb.s ! VPastPart APred) ;

      vHaben = auxPerfect verb ;
      hat : Mood -> Tense -> VAgr -> Str = \m,t,a ->
        vHaben ! vFin False m t a ;
      haben : Str = vHaben ! VInf False ;

      wird : Mood -> VAgr -> Str = \m,a ->
        let
          an : Number = numberAgr a ;
          ap : Person = personAgr a ;
        in
        case m of {
          MIndic => werden_V.s ! VFin False (VPresInd an ap) ; 
          MConjunct => werden_V.s ! VFin False (VPresSubj an ap)
        } ;
      wuerde : VAgr -> Str = \a ->                    --# notpresent
        werden_V.s ! VFin False (VImpfSubj (numberAgr a) (personAgr a)) ;  --# notpresent

      auf = verb.prefix ;
      vf : Bool -> Str -> Str -> Str -> {fin,inf,inf2 : Str} = \b,fin,inf,inf2 -> {
        fin = fin ; 
        inf = verb.particle ++ if_then_Str b [] auf ++ inf ; --- negation of main b
        inf2 = inf2
        } ; 
    in {
    s = \\b,a => table {
      VPFinite m t Simul => case t of {
--        Pres | Past => vf (vfin m t a) [] ; -- the general rule
        Past => vf b (vfin b m t a) [] [] ;    --# notpresent
        Fut  => vf True (wird m a) vinf [] ;   --# notpresent
        Cond => vf True (wuerde a) vinf [] ;   --# notpresent
        Pres => vf b (vfin b m t a) [] []
        } ;
      VPFinite m t Anter => case t of {
        Past => vf True (hat m t a) vpart [] ;           --# notpresent
        Fut  => vf True (wird m a) vpart haben ;         --# notpresent
        Cond => vf True (wuerde a) vpart haben ;         --# notpresent
        Pres => vf True (hat m t a) vpart []
        } ;
      VPImperat False => vf False (verb.s ! VImper (numberAgr a)) [] [] ;
      VPImperat True  => vf False (verb.s ! VFin False (VPresSubj Pl P3)) [] [] ;
      VPInfinit Anter => vf True [] (vpart ++ haben) [] ;   --# notpresent
      VPInfinit Simul => vf True [] (verb.s ! VInf b) []
      }
    } ;

  predV : Verb -> VP = predVGen False ;

  predVc : Verb ** {c2 : Preposition} -> VPSlash = \v ->
    predV v ** {c2 = v.c2 ; objCtrl = False} ;

  predVGen : Bool -> Verb -> VP = \isAux, verb -> {
    s = verb ;
    a1,a2  : Str = [] ;
    nn  : Agr => Str * Str * Str * Str = case verb.vtype of {
      VAct => \\_ => <[],[],[],[]> ;
      VRefl c => \\a => <reflPron ! a ! c,[],[],[]>
      } ;
    isAux = isAux ; ----
    -- default infinitival complement:
    inf = {inpl = <\\_ => [], []>; extr = \\_ => []} ;
    ext,adj : Str = [] ;
    c1 = PrepNom
    } ;

  auxPerfect : Verb -> VForm => Str = \verb ->
    case verb.aux of {
      VHaben => haben_V.s ;
      VSein => sein_V.s
      } ;

  haben_V : Verb = 
    mkV 
      "haben" "habe" "hast" "hat" "habt" "hab" 
      "hatte" "hattest" "hatten" "hattet" 
      "hätte" "gehabt" 
      [] VHaben ;

  werden_V : Verb = 
    mkV 
      "werden" "werde" "wirst" "wird" "werdet" "werd" 
      "wurde" "wurdest" "wurden" "wurdet" 
      "würde" "geworden" 
      [] VSein ;

  werdenPass : Verb = 
    mkV 
      "werden" "werde" "wirst" "wird" "werdet" "werd" 
      "wurde" "wurdest" "wurden" "wurdet" 
      "würde" "worden" 
      [] VSein ;

  sein_V : Verb = 
    let
      sein = mkV 
      "sein" "bin" "bist" "ist" "seid" "sei" 
      "war"  "warst" "waren" "wart" 
      "wäre" "gewesen" 
      [] VSein
    in
    {s = table {
      VFin _ (VPresInd Pl (P1 | P3)) => "sind" ;
      VFin _ (VPresSubj Sg P2) => "seiest" ; --- (variants {"seiest" ; "seist"}) ; -- no variants in the RGL
      VFin _ (VPresSubj Sg _)  => "sei" ;
      VFin _ (VPresSubj Pl P2) => "seiet" ;
      VFin _ (VPresSubj Pl _)  => "seien" ;
      VPresPart a => (regA "seiend").s ! Posit ! a ;
      v => sein.s ! v 
      } ;
     prefix = [] ;
     particle = [] ;
     aux = VSein ;
     vtype = VAct 
    } ;

  auxVV : Verb -> Verb ** {isAux : Bool} = \v -> v ** {isAux = True} ;

  negation : Polarity => Str = table {
      Pos => [] ;
      Neg => "nicht"
      } ;

  -- IL 24/04/2018 Fixing the scope of reflexives
  objAgr : { a : Agr } -> VP -> VP = \obj,vp -> vp ** {
    nn = \\a => vp.nn ! obj.a ;
    inf = {inpl = <\\a => vp.inf.inpl.p1 ! obj.a, vp.inf.inpl.p2> ;  -- HL 3/2022
           extr = \\a => vp.inf.extr ! obj.a} } ;                    -- HL 3/2022

-- Extending a verb phrase with new constituents.

  insertObj : (Agr => Str) -> VP -> VP = \obj,vp ->  -- obj:Comp A|Adv|CN
    vp ** { nn = \\a => let vpnn = vp.nn ! a in
                        <vpnn.p1, vpnn.p2, vpnn.p3, obj ! a ++ vpnn.p4> } ;

  insertObjc : (Agr => Str) -> VPSlash -> VPSlash = \obj,vp ->
    insertObj obj vp ** {c2 = vp.c2 ; objCtrl = vp.objCtrl } ;

  insertObjNP : NP -> Preposition -> VPSlash -> VPSlash = \np,prep,vp ->
    let obj = appPrepNP prep np ;
        b : Bool = case prep.t of {isPrep | isPrepDefArt => True ; _ => False} ;
        w = np.w ;
        c = prep.c
    in insertObj' obj b w c vp ;

  insertObj' : Str -> Bool -> Weight -> Case -> VPSlash -> VPSlash = \obj,isPrep,w,c,vp ->
    vp ** {
      nn = \\a =>
        let vpnn = vp.nn ! a in
        -- HL 11/6/19: rough object NP order (expensive):
        -- vfin < accPron < refl < (gen|dat)Pron < lightNP < neg < heavyNP|PP < vinf|comp
        case <isPrep, w, c> of { -- 2 * 3 * 4 = 24 cases
          <True, _,_> =>       -- <prons, light, heavy++pp, compl>
            <vpnn.p1, vpnn.p2, vpnn.p3 ++ obj, vpnn.p4> ;
          <False,WPron, Acc> => -- <ihn ++ sich, light, heavy, comp>
            <obj ++ vpnn.p1, vpnn.p2, vpnn.p3, vpnn.p4> ;
          <False,WPron, _  > => -- <sich ++ ihm|seiner, light, heavy, comp>
            <vpnn.p1 ++ obj, vpnn.p2, vpnn.p3, vpnn.p4> ;
          <False,WLight,Dat> => -- (assuming v.c2=acc) nonPron: dat < acc|gen
                                -- <prons, dat ++ np, heavy, comp>
            <vpnn.p1, obj ++ vpnn.p2, vpnn.p3, vpnn.p4> ;
          <False,WLight,_  > => -- <prons, np ++ gen|acc, heavy, comp>
            <vpnn.p1, vpnn.p2 ++ obj, vpnn.p3, vpnn.p4> ;
          <False,WHeavy|WDefArt,Dat> => -- <prons, light, dat ++ np, comp>
            <vpnn.p1, vpnn.p2, obj ++ vpnn.p3, vpnn.p4> ;
          <False,WHeavy|WDefArt,_  > => -- <prons, light, np ++ gen|acc, comp>
            <vpnn.p1, vpnn.p2, vpnn.p3 ++ obj, vpnn.p4> }
    } ; -- the ordering of objects of v:V3 (and v:V4) is also determined by Slash?V3 (and Slash?V4)

  insertObjRefl : VPSlash -> VPSlash = \vp -> -- HL 6/2019, to order reflPron < neg < prep+reflPron
    let prep = vp.c2 ;
        obj : Agr => Str = \\a => prep.s ! GPl ++ reflPron ! a ! prep.c ++ prep.s2
    in vp ** {
      nn = \\a =>
        let vpnn = vp.nn ! a in
        case prep.t of {
          isCase => <obj ! a ++ vpnn.p1, vpnn.p2, vpnn.p3, vpnn.p4> ;
          _      => <vpnn.p1, obj ! a ++ vpnn.p2, vpnn.p3, vpnn.p4> }
    } ;

  insertAdV : Str -> VP -> VP = \adv,vp -> vp ** { -- not used in Ger, so VP.a1 can be skipped
    a1 = adv ++ vp.a1 } ;                          -- cf. AdvVP(Slash),AdVVP(Slash)

  insertAdv : Str -> VP -> VP = \adv,vp -> vp ** {
    a2 = vp.a2 ++ adv } ;

  insertExtrapos : Str -> VP -> VP = \ext,vp -> vp ** {
    ext = vp.ext ++ ext } ;

  -- HL 3/2022: to do nested infinitival objects in ComplVV, SlashVV, SlashV2V
  -- embed <sich, helfen> into <ihn, lassen> = <ihn sich, helfen lassen>
  embedInf : (Agr => Str) * Str -> (Agr => Str) * Str -> (Agr => Str) * Str =
     \f,g -> <\\a => g.p1!a ++ f.p1!a, f.p2 ++ g.p2> ;

  -- Futur-II:  (ich werde) ihn dir ++ haben ++ helfen lassen
  insertInf : {inpl:(Agr => Str)*Str ; extr:(Agr => Str)} -> VP -> VP =
  \inf,vp -> vp ** {inf = {inpl = embedInf inf.inpl vp.inf.inpl ;
                           extr = \\agr => vp.inf.extr!agr ++ inf.extr!agr}} ;

  glueInpl : (Agr => Str)*Str -> (Agr => Str) =
      \inplace -> \\agr => (inplace.p1!agr ++ inplace.p2) ;

  -- HL 3/22: extract infzu-complement, leave inf-complement in-place
  mkInf : Bool -> Anteriority -> Polarity -> VP ->
                {inpl : (Agr => Str) * Str ; extr : (Agr => Str)} =
     \isAux,ant,pol,vp ->
        let
            vpi = infVP isAux ant pol vp ;
            topInpl = <vpi.objs, vpi.pred> ;
            emptyInpl : (Agr => Str) * Str = <\\_ => [], []> ;
            comma = bindComma
        in
          case <isAux,vp.isAux> of {
            <True,True> -- 1: {s=will, inpl=<(sich, waschen) können>, extr = []}
                => {inpl = embedInf vpi.inpl topInpl ;
                    extr = \\agr => vpi.extr!agr} ;
            <True,False> -- 2: {s=will; inpl=<[], versuchen>, extr = sich zu waschen}
                => {inpl = topInpl ;
                    extr = \\agr => (glueInpl vpi.inpl)!agr ++ vpi.extr!agr} ;
            <False,True> -- 3: {s=wagt; inpl=<[], []>, extr = (sich, waschen) zu wollen}
                => {inpl = emptyInpl ;
                    extr = let moved = embedInf vpi.inpl topInpl
                           in \\agr => comma ++ (glueInpl moved)!agr ++ vpi.extr!agr} ;
            <False,False> -- 4: {s=wagt, inpl=<[], []>, extr = zu versuchen, (sich zu waschen)}
                => {inpl = emptyInpl ;
                    extr = \\agr => comma ++ (glueInpl topInpl)!agr ++ vpi.extr!agr}
        } ;

  insertAdj : Str -> Str * Str -> Str -> VP -> VP = \adj,c,ext,vp -> vp ** {
    nn = \\a => 
      let vpnn = vp.nn ! a in <vpnn.p1, vpnn.p2 ++ c.p1, -- der Frau treu
                               vpnn.p3, vpnn.p4> ;
    adj = vp.adj ++ adj ++ c.p2 ;                        -- neugierig auf das Buch
    ext = vp.ext ++ ext} ;           

--------------------------------------------
--CLAUSE CONSTRUCTION
--------------------------------------------

-- For $Sentence$.

  Clause : Type = {
    s : Mood => Tense => Anteriority => Polarity => Order => Str
    } ;

  mkClause : Str -> Agr -> VP -> Clause = \subj,agr,vp ->
    let vagr = agr2vagr agr ;
         vps = useVP vp in {
      s = \\m,t,a,b,o =>
        let
          ord   = case o of {
            Sub => True ;  -- glue prefix to verb
            _ => False
            } ;
          verb  = vps.s  ! ord ! vagr ! VPFinite m t a ;
          haben = verb.inf2 ;
          neg = negation ! b ;
          obj1  = (vp.nn ! agr).p1 ++ (vp.nn ! agr).p2 ; -- refl ++ pronouns ++ light nps
          obj2  = (vp.nn ! agr).p3 ;                     -- pp-objects and heavy nps
          obj3  = (vp.nn ! agr).p4 ++ vp.adj ++ vp.a2 ;  -- pred.AP|CN|Adv, via useComp HL 6/2019
          compl = obj1 ++ neg ++ obj2 ++ obj3 ;
          infObjs = (vp.inf.inpl.p1)!agr ;
          infPred = vp.inf.inpl.p2 ;
          -- leave inf-complement of +auxV(2)V in place,
          -- extract infzu-complement of -auxV(2)V: (ComplVV, SlashV2V)
          infCompl : Str = case <t,a,vp.isAux> of {
            <Fut|Cond,Anter,True> => [] ;                                 --# notpresent
            _ => infObjs ++ infPred } ;
          pred : {inf, infComplfin : Str} = case <t,a,vp.isAux> of {
             <Fut|Cond,Anter,True>  =>                                    --# notpresent
               {inf    = infObjs ++ haben ++ infPred ++ verb.inf ;        --# notpresent Duden 318
                infComplfin = -- es ++ wird ++ haben ++ tun ++ wollen     --# notpresent
                   infObjs ++ verb.fin ++ haben ++ infPred ++ verb.inf} ; --# notpresent
             <_,Anter,True> =>                                            --# notpresent
               {inf    = verb.inf ++ haben ;                              --# notpresent
                infComplfin = -- es ++ wird/hat/hatte ++ tun ++ wollen    --# notpresent
                   infObjs ++ verb.fin ++ infPred ++ verb.inf ++ haben} ; --# notpresent
              _ =>
               {inf    = verb.inf ++ haben ;
                infComplfin = -- es zu tun ++ versucht/[] +[]+ hat/versuchte
                   infCompl ++ verb.inf ++ haben ++ verb.fin}
              } ;
          extra = vp.inf.extr!agr ++ vp.ext ;
        in
        case o of {
	  Main => subj ++ verb.fin ++ compl ++ infCompl ++ pred.inf ++ extra ;
	  Inv  => verb.fin ++ subj ++ compl ++ infCompl ++ pred.inf ++ extra ;
	  Subj =>             subj ++ compl ++   pred.infComplfin   ++ extra
        }
    } ;

{-
-- tests 27/5/2012

  ich bin nicht alt
  ich bin nicht hier
  ich kenne dich nicht
  ich kenne deine Frau nicht
  ich bin nicht ein Kind / ich bin kein Kind (via no_Quant)
  ich schlafe nicht hier
  ich sage nicht, dass es regnet
  ich male es nicht blau
  ich schlafe nicht immer
  ich kenne dich nicht immer
  ich kann nicht schlafen
  es wird nicht besser
-}

  infVP = overload {
     infVP : Bool -> VP -> ((Agr => Str) * Str * Str * Str)
     = infVP_orig ; -- from gf-3.9,
     infVP : Bool -> Anteriority -> Polarity -> VP
           -> { objs:(Agr => Str) ; pred:Str; inpl:(Agr=>Str)*Str ; extr:(Agr=>Str) }
     = infVP_ant ; -- admit infinitive in Anter anteriority and Neg polarity
  } ;

  infVP_orig : Bool -> VP -> ((Agr => Str) * Str * Str * Str) =
    \isAux, vp -> let vps = useVP vp ;
                      infExt = vp.inf.extr ! agrP3 Sg   -- HL 3/22
    in
    <
     \\agr => (vp.nn ! agr).p1 ++ (vp.nn ! agr).p2 ++ (vp.nn ! agr).p3 ++ (vp.nn ! agr).p4 ++ vp.a2,
     vp.a1 ++ vp.adj ++ (vps.s ! (notB isAux) ! vagrP3 Sg ! VPInfinit Simul).inf,  -- vp.a1 ! Pos
     vp.inf.inpl.p2, -- ! HL
     infExt ++ vp.ext
    > ;

  infVP_ant : Bool -> Anteriority -> Polarity -> VP  -- HL 3/22
    -> { objs:(Agr => Str) ; pred:Str ; inpl:(Agr=>Str)*Str ; extr:(Agr=>Str) } =
      \isAux, ant, pol, vp -> let vps = useVP vp in
      {
        objs = \\agr => (vp.nn ! agr).p1 ++ (vp.nn ! agr).p2 ++ negation ! pol ++ (vp.nn ! agr).p3
          ++ vp.a2 ++ (vp.nn ! agr).p4 ;  -- objects + predicative A|CN|NP
        pred = vp.a1 ++ vp.adj ++ (vps.s ! (notB isAux) ! vagrP3 Sg ! VPInfinit ant).inf ;
        -- inplace and extracted parts of vp.inf:
        inpl = vp.inf.inpl ;
        extr = vp.inf.extr
      } ;

  infVPSlash : Bool -> Anteriority -> Polarity -> VPSlash  -- HL 3/22
    -> { objs:(Agr => Str) ; pred:Str; inpl:(Agr=>Str)*Str ; extr:(Agr=>Str) } =
      \isAux, ant, pol, vp -> let vps = useVP vp in
      { objs = \\agr => (vp.nn ! agr).p1 ++ (vp.nn ! agr).p2 ++ negation ! pol ++ (vp.nn ! agr).p3
          ++ vp.a2 ++ (vp.nn ! agr).p4 ;  -- objects + predicative A|CN|NP
        pred = vp.inf.inpl.p2 ++ vp.a1 ++ vp.adj ++ (vps.s ! (notB isAux) ! vagrP3 Sg ! VPInfinit ant).inf ;
        -- inplace and extracted parts of vp.inf:
        inpl = <vp.inf.inpl.p1, []> ; -- move the predicate part to pred
        extr = vp.inf.extr
      } ** {c2 = vp.c2 ; objCtrl = vp.objCtrl} ;

  -- for CatGer.linref VP and Verb.embedVP:
  useInfVP : Bool -> VP -> Str = \isAux,vp ->
    -- let vpi = infVP isAux vp in
    -- vpi.p1 ! agrP3 Sg ++ vpi.p3 ++ vpi.p2 ++ vpi.p4 ;
    let vpi = infVP isAux Simul Pos vp ; -- HL 3/2022
        agr = agrP3 Sg ;
        glue : (Agr => Str)*Str -> Str =  \i -> i.p1 ! agr ++ i.p2
    in
       glue (embedInf vpi.inpl <vpi.objs, vpi.pred>) ++ vpi.extr!agr ++ vp.ext ;

-- The nominative case is not used as reflexive, but defined here
-- so that we can reuse this in personal pronouns. 

  reflPron : Agr => Case => Str = table { -- with persPron nominative
    AgSgP1       => caselist "ich" "mich" "mir"  "meiner" ;
    AgSgP2       => caselist "du"  "dich" "dir"  "deiner" ;
    AgSgP3 Masc  => caselist "er" "sich" "sich" "seiner" ;
    AgSgP3 Fem   => caselist "sie" "sich" "sich" "ihrer" ;
    AgSgP3 Neutr => caselist "es" "sich" "sich" "seiner" ;
    AgPl P1      => caselist "wir" "uns"  "uns"  "unser" ;
    AgPl P2      => caselist "ihr" "euch" "euch" "euer" ;
    AgPl P3      => caselist "sie" "sich" "sich" "ihrer" ;
    AgPlPol      => caselist "Sie" "sich" "sich" "Ihrer"              -- HL 8/2023
    -- AgSgP3Gen    => caselist "man selbst" "sich" "sich" "seiner" ; -- älter als man selbst sein
    -- ; AgPlReci  => caselist "man" "einander" "einander" "einander" -- reciPron ?
    } ;

  possPron : Agr -> Number -> Gender -> Case -> Str = \a,n,g,c -> case <a,n,g> of {
    <AgSgP1,Sg,Masc>  => caselist "mein"  "meinen" "meinem"  "meines" ! c ;
    <AgSgP1,Sg,Fem>   => caselist "meine" "meine"  "meiner"  "meiner" ! c ;
    <AgSgP1,Sg,Neutr> => caselist "mein"  "mein"   "meinem"  "meines" ! c ;
    <AgSgP1,Pl,_>     => caselist "meine" "meine"  "meinen"  "meiner" ! c ;
    <AgSgP2,Sg,Masc>  => caselist "dein"  "deinen" "deinem"  "deines" ! c ;
    <AgSgP2,Sg,Fem>   => caselist "deine" "deine"  "deiner"  "deiner" ! c ;
    <AgSgP2,Sg,Neutr> => caselist "dein"  "dein"   "deinem"  "deines" ! c ;
    <AgSgP2,Pl,_>     => caselist "deine" "deine"  "deinen"  "deiner" ! c ;

    <AgSgP3 Fem,Sg,Masc>  => caselist "ihr"  "ihren" "ihrem"  "ihres" ! c ;
    <AgSgP3 Fem,Sg,Fem>   => caselist "ihre" "ihre"  "ihrer"  "ihrer" ! c ;
    <AgSgP3 Fem,Sg,Neutr> => caselist "ihr"  "ihr"   "ihrem"  "ihres" ! c ;
    <AgSgP3 Fem,Pl,_>     => caselist "ihre" "ihre"  "ihren"  "ihrer" ! c ;

    <AgSgP3 _,Sg,Masc>  => caselist "sein"  "seinen" "seinem"  "seines" ! c ;
    <AgSgP3 _,Sg,Fem>   => caselist "seine" "seine"  "seiner"  "seiner" ! c ;
    <AgSgP3 _,Sg,Neutr> => caselist "sein"  "sein"   "seinem"  "seines" ! c ;
    <AgSgP3 _,Pl,_>     => caselist "seine" "seine"  "seinen"  "seiner" ! c ;

    <AgPl P1,Sg,Masc>  => caselist "unser"  "unseren" "unserem"  "unseres" ! c ;
    <AgPl P1,Sg,Fem>   => caselist "unsere" "unsere"  "unserer"  "unserer" ! c ;
    <AgPl P1,Sg,Neutr> => caselist "unser"  "unser"   "unserem"  "unseres" ! c ;
    <AgPl P1,Pl,_>     => caselist "unsere" "unsere"  "unseren"  "unserer" ! c ;

    <AgPl P2,Sg,Masc>  => caselist "euer" "euren" "eurem"  "eures" ! c ;
    <AgPl P2,Sg,Fem>   => caselist "eure" "eure"  "eurer"  "eurer" ! c ;
    <AgPl P2,Sg,Neutr> => caselist "euer" "euer"  "eurem"  "eures" ! c ;
    <AgPl P2,Pl,_>     => caselist "eure" "eure"  "euren"  "eurer" ! c ;

    <AgPl P3,Sg,Masc>  => caselist "ihr"  "ihren" "ihrem"  "ihres" ! c ;
    <AgPl P3,Sg,Fem>   => caselist "ihre" "ihre"  "ihrer"  "ihrer" ! c ;
    <AgPl P3,Sg,Neutr> => caselist "ihr"  "ihr"   "ihrem"  "ihres" ! c ;
    <AgPl P3,Pl,_>     => caselist "ihre" "ihre"  "ihren"  "ihrer" ! c ;

    <AgPlPol,Sg,Masc>  => caselist "Ihr"  "Ihren" "Ihrem"  "Ihres" ! c ;
    <AgPlPol,Sg,Fem>   => caselist "Ihre" "Ihre"  "Ihrer"  "Ihrer" ! c ;
    <AgPlPol,Sg,Neutr> => caselist "Ihr"  "Ihr"   "Ihrem"  "Ihres" ! c ;
    <AgPlPol,Pl,_>     => caselist "Ihre" "Ihre"  "Ihren"  "Ihrer" ! c
    } ;

  conjThat : Str = "dass" ;

  conjThan : Str = "als" ;

-- The infinitive particle "zu" is used if and only if $vv.isAux = False$.
 
  infPart : Bool -> Str = \b -> if_then_Str b [] "zu" ;

  heavyNP : 
    {s : Bool => Case => Str ; a : Agr} -> {s : Bool => Case => Str ; a : Agr ; w : Weight ; ext,rc : Str} = \np ->
    np ** {w = WHeavy ; ext,rc = []} ; -- this could be wrong

  relPron :  RelGenNum => Case => Str = \\rgn,c =>
    case rgn of {
    RGenNum gn => 
      case <gn,c> of {
      <GSg Fem,Gen> => "deren" ;
      <GSg g,Gen>   => "dessen" ;
      <GPl,Dat>     => "denen" ;
      <GPl,Gen>     => "deren" ;
      _ => artDef ! gn ! c
      } ;
    RSentence => (caselist "was" "was" "was" "wessen") ! c   -- wessen HL 4/2022
    } ;

-- Function that allows the construction of non-nominative subjects.
  mkSubject : NP -> Preposition -> {s:Str ; a:Agr} = \np, prep ->
    let
      agr = case prep.c of { Nom => np.a ; _ => AgSgP3 Masc } ;
      subj = appPrepNP prep np
    in {s = subj ; a = agr} ;

  sex2gender : Sex -> Gender = \g ->
    case g of {
      Male => Masc ;
      Female => Fem
    } ;

}
