--# -path=.:../abstract:../common:../../prelude

--1 Finnish Lexical Paradigms
--
-- Aarne Ranta 2003--2008
--
-- This is an API to the user of the resource grammar
-- for adding lexical items. It gives functions for forming
-- expressions of open categories: nouns, adjectives, verbs.
--
-- Closed categories (determiners, pronouns, conjunctions) are
-- accessed through the resource syntax API and $Structural.gf$.
--
-- The main difference with $MorphoFin.gf$ is that the types
-- referred to are compiled resource grammar types. We have moreover
-- had the design principle of always having existing forms, rather
-- than stems, as string arguments of the paradigms.
--
-- The structure of functions for each word class $C$ is the following:
-- there is a polymorphic constructor $mkC$, which takes one or
-- a few arguments. In Finnish, one argument is enough in 80-90% of
-- cases in average.

resource ParadigmsFin = Kotus ** open
  (Predef=Predef),
  Prelude,
  MorphoFin,
  CatFin, StemFin
  in {

  flags optimize=noexpand ;
    coding=utf8 ;

--2 Parameters
--
-- To abstract over gender, number, and (some) case names,
-- we define the following identifiers. The application programmer
-- should always use these constants instead of the constructors
-- defined in $ResFin$.

oper
  Number   : Type ;

  singular : Number ;
  plural   : Number ;

  Case        : Type ;
  nominative  : Case ; -- e.g. "talo"
  genitive    : Case ; -- e.g. "talon"
  partitive   : Case ;  -- e.g. "taloa"
  essive      : Case ;  -- e.g. "talona"
  translative : Case ;  -- e.g. "taloksi"
  inessive    : Case ;  -- e.g. "talossa"
  elative     : Case ;  -- e.g. "talosta"
  illative    : Case ;  -- e.g. "taloon"
  adessive    : Case ;  -- e.g. "talolla"
  ablative    : Case ;  -- e.g. "talolta"
  allative    : Case ; -- e.g. "talolle"

  infFirst : InfForm ; -- e.g. "tehdä"
  infIness : InfForm ; -- e.g. "tekemässä"
  infElat  : InfForm ; -- e.g. "tekemästä"
  infIllat : InfForm ; -- e.g. "tekemään"
  infAdess : InfForm ; -- e.g. "tekemällä"
  infPart  : InfForm ; -- e.g. "tekemistä"
  infPresPart : InfForm ; -- e.g. "tekevän"
  infPresPartAgr : InfForm ; -- e.g. "tekevänsä"

-- The following type is used for defining *rection*, i.e. complements
-- of many-place verbs and adjective. A complement can be defined by
-- just a case, or a pre/postposition and a case.

  prePrep     : Case -> Str -> Prep ;  -- preposition, e.g. partitive "ilman"
  postPrep    : Case -> Str -> Prep ;  -- postposition, e.g. genitive "takana"
  postGenPrep :         Str -> Prep ;  -- genitive postposition, e.g. "takana"
  casePrep    : Case ->        Prep ;  -- just case, e.g. adessive

  mkPrep = overload {
    mkPrep : Case -> Prep
     = casePrep ;
    mkPrep : Str -> Prep
     = postGenPrep ;
    mkPrep : Case -> Str -> Prep
     = postPrep ;
    mkPrep : Str -> Case -> Prep
     = \s,c -> prePrep c s ;
    } ;

  accusative : Prep
    = lin Prep {c = NPAcc ; s = <[],[],\\_ => []>} ;

  NK : Type ;   -- Noun from DictFin (Kotus)
  AK : Type ;   -- Adjective from DictFin (Kotus)
  VK : Type ;   -- Verb from DictFin (Kotus)
  AdvK : Type ; -- Adverb from DictFin (Kotus)

--2 Nouns

-- The worst case gives ten forms.
-- In practice just a couple of forms are needed to define the different
-- stems, vowel alternation, and vowel harmony.

oper

-- The regular noun heuristic takes just one form (singular
-- nominative) and analyses it to pick the correct paradigm.
-- It does automatic grade alternation, and is hence not usable
-- for words like "auto" (whose genitive would become "audon").
--
-- If the one-argument paradigm does not give the correct result, one can try and give
-- two or three forms. Most notably, the two-argument variant is used
-- for nouns like "kivi - kiviä", which would otherwise become like
-- "rivi - rivejä". Three arguments are used e.g. for
-- "auto - auton - autoja", which would otherwise become
-- "auto - audon".

  mkN : overload {
    mkN : (kukko : Str) -> N ;  -- predictable nouns, covers 82%
    mkN : (savi,savia : Str) -> N ; -- different pl.part
    mkN : (vesi,veden,vesiä : Str) -> N ; -- also different sg.gen
    mkN : (vesi,veden,vesiä,vettä : Str) -> N ; -- also different sg.part
    mkN : (olo,n,a,na,oon,jen,ja,ina,issa,ihin : Str) -> N ; -- worst case, 10 forms
    mkN : (pika : Str) -> (juna  : N) -> N ; -- compound with invariable prefix
    mkN : (oma : N)    -> (tunto : N) -> N ; -- compound with inflecting prefix
    mkN : NK -> N ;  -- noun from DictFin (Kotus)
    mkN : V -> N ;   -- verbal noun: "tekeminen"
  } ;

-- Some nouns are regular except for the singular nominative (e.g. "mies").

    exceptNomN : N -> Str -> N ;

-- Nouns where the parts are separate (should perhaps be treated as CN)

   separateN = overload {
     separateN : Str -> N -> N
      = \s,n -> mkN (s + "_") n ;
     separateN : N -> N -> N
      = \oma, asunto -> lin N {s = \\c => oma.s ! c + "_" + asunto.s ! c ; h = asunto.h} ;
      } ;

   compN : N -> N -> N = \valkuainen,aine -> aine ** {
     s = \\c => (StemFin.snoun2nounBind valkuainen).s ! NCompound + aine.s ! c
     } ;

   genCompN = overload {
     genCompN : N -> N -> N = genitiveCompoundN Sg ;
     genCompN : Number -> N -> N -> N = genitiveCompoundN
     } ;

   genitiveCompoundN : Number -> N -> N -> N = \n,veri,paine -> paine ** {
     s = \\c => (StemFin.snoun2nounBind veri).s ! NCase n Gen + paine.s ! c
     } ;

-- Nouns used as functions need a case, of which the default is
-- the genitive.

  mkN2 : overload {
    mkN2 : N -> N2 ;        -- relational noun with genitive
    mkN2 : N -> Prep -> N2  -- relational noun another prep.
    } ;

  mkN3  : N -> Prep -> Prep -> N3 ; -- relation with two complements

-- Proper names can be formed by using declensions for nouns.
-- The plural forms are filtered away by the compiler.

  mkPN : overload {
    mkPN : Str -> PN ;  -- predictable noun made into name
    mkPN : N -> PN      -- any noun made into name
    } ;

-- A special function for foreign names: no grade alternation, no final aspiration.

  foreignPN : Str -> PN ; -- Dieppe-Dieppen

--2 Adjectives

-- Non-comparison one-place adjectives are just like nouns.
-- The regular adjectives are based on $regN$ in the positive.
-- Comparison adjectives have three forms.
-- The comparative and the superlative
-- are always inflected in the same way, so the nominative of them is actually
-- enough (except for the superlative "paras" of "hyvä").

  mkA : overload {
    mkA : Str -> A ;  -- regular noun made into adjective
    mkA : N -> A ;    -- any noun made into adjective
    mkA : N -> (kivempi,kivin : Str) -> A ; -- deviating comparison forms
    mkA : (hyva,prmpi,pras : N) -> (hyvin,pmmin,prhten : Str) -> A ; -- worst case adj
    mkA : AK -> A ;  -- adjective from DictFin (Kotus)
  } ;

  invarA : Str -> A  -- invariant adjective, e.g. "kelpo"
    = \s -> lin A {s = \\_,_ => s ; h = Back ; p = [] ; hasPrefix = False} ; ----- stemming adds bogus endings

  prefixA : Str -> A -> A = \pr,a -> a ** {
    p = pr ;
    hasPrefix = True
    } ;

-- Two-place adjectives need a case for the second argument.

  mkA2 = overload {
    mkA2 : Str -> A2  -- e.g. "vihainen" (jollekin)
      = \a -> let adj = mkA a ;
               in lin A2 (adj ** {c2 = casePrep allative}) ;
    mkA2 : Str -> Prep -> A2  -- e.g. "jaollinen" (mkPrep adessive)
    = \a,p ->  let adj = mkA a in lin A2 (adj ** {c2=p}) ;
    mkA2 : A -> Prep -> A2  -- e.g. "jaollinen" (mkPrep adessive)
    = \a,p -> lin A2 (a ** {c2 = p}) ;
    } ;


--2 Verbs
--
-- The grammar does not cover the potential mood and some nominal
-- forms. One way to see the coverage is to linearize a verb to
-- a table.
-- The worst case needs twelve forms, as shown in the following.

  mkV : overload {
    mkV : (huutaa : Str) -> V ;     -- predictable verbs, covers 90%
    mkV : (huutaa,huusi : Str) -> V ; -- deviating past 3sg
    mkV : (huutaa,huudan,huusi : Str) -> V ; -- also deviating pres. 1sg
    mkV : (huutaa,dan,taa,tavat,takaa,detaan,sin,si,sisi,tanut,dettu,tanee : Str) -> V ; -- worst-case verb
    mkV : VK -> V ;  -- verb from DictFin (Kotus)
    mkV : V -> Str -> V ; -- hakata päälle (particle verb)
    mkV : Str -> V -> V ; -- laimin+lyödä (prefixed verb)
  } ;

-- All the patterns above have $nominative$ as subject case.
-- If another case is wanted, use the following.

  caseV : Case -> V -> V ;  -- deviating subj. case, e.g. genitive "täytyä"

-- The verbs "be" is special.

  vOlla : V ; -- the verb "be"

  olla_V : V
   = vOlla ;

--3 Two-place verbs
--
-- Two-place verbs need an object case, and can have a pre- or postposition.
-- The default is direct (accusative) object. There is also a special case
-- with case only. The string-only argument case yields a regular verb with
-- accusative object.

  mkV2 : overload {
    mkV2 : Str -> V2 ;  -- predictable direct transitive
    mkV2 : Str -> Case -> V2 ;  -- predictable with another case
    mkV2 : V -> V2 ;    -- direct transitive
    mkV2 : V -> Case -> V2 ; -- complement just case
    mkV2 : V -> Prep -> V2 ; -- complement pre/postposition
    mkV2 : VK -> V2 ;    -- direct transitive of Kotus verb
    } ;


--3 Three-place verbs
--
-- Three-place (ditransitive) verbs need two prepositions, of which
-- the first one or both can be absent.

  mkV3 = overload {
   mkV3 : Str -> V3
     = \s -> dirdirV3 (mkV s) ;
   mkV3 : V -> V3
     = \v -> dirdirV3 v ;
   mkV3 : V -> Prep -> Prep -> V3  -- e.g. puhua, allative, elative
     = \v,p,q -> v ** {c2 = p ; c3 = q ; lock_V3 = <>} ;
   } ;

  dirV3    : V -> Case -> V3 ;          -- siirtää, (accusative), illative
  dirdirV3 : V         -> V3 ;          -- antaa, (accusative), (allative)


--3 Other complement patterns
--
-- Verbs and adjectives can take complements such as sentences,
-- questions, verb phrases, and adjectives.

mkVV = overload {
  mkVV : Str -> VV   -- e.g. "yrittää" (puhua)
   = \s -> mkVVf (mkV s) infFirst ;
  mkVV : V -> VV     -- e.g. "alkaa" (puhua)
   = \v -> mkVVf v infFirst ;
  mkVV : Str -> InfForm -> VV  -- e.g. "ruveta" (puhumaan)
   = \s,i -> mkVVf (mkV s) i ;
  mkVV : V -> InfForm -> VV  -- e.g. "lakata" (puhumasta)
   = \v,i -> mkVVf v i ;
  } ;

mkVS = overload {
  mkVS : Str -> VS   -- e.g. "väittää"
   = \s -> lin VS (mk1V s) ;
  mkVS : V -> VS     -- e.g. "sanoa"
   = \v -> lin VS v ;
  } ;

  mkV2V = overload {
    mkV2V : Str -> V2V  -- reg verb, partitive + infIllat
     = \s -> mkV2Vf (mkV s) (casePrep partitive) infIllat ; ----
    mkV2V : V -> V2V    -- partitive + infillat
     = \v -> mkV2Vf v (casePrep partitive) infIllat ; ----
    mkV2V : V -> Prep -> V2V  -- e.g. "käskeä" genitive + infFiilat
     = \v,p -> mkV2Vf v p infIllat ;
    mkV2V  : V -> Prep -> InfForm -> V2V -- e.g. "kieltää" partitive infElat
     = \v,p,f -> mk2V2 v p ** {vi = infform2vvtype f ; lock_V2V = <>} ;
    mkV2V  : V -> Case -> InfForm -> V2V
     = \v,c,f -> mk2V2 v (casePrep c) ** {vi = infform2vvtype f ; lock_V2V = <>} ;
    } ;

  mkV0  : V -> V0 ; --%

  mkV2S : V -> Prep -> V2S ; -- e.g. "sanoa" allative
  mkVVf : V -> InfForm -> VV ; -- e.g. "ruveta" infIllat
  mkV2Vf : V -> Prep -> InfForm -> V2V ; -- e.g. "kieltää" partitive infElat
  mkVA  : V -> Prep -> VA ; -- e.g. "maistua" ablative
  mkV2A : V -> Prep -> Prep -> V2A ; -- e.g. "maalata" accusative translative
  mkVQ  : V -> VQ ;
  mkV2Q : V -> Prep -> V2Q ; -- e.g. "kysyä" ablative

  mkAS  : A -> AS ; --%
---  mkA2S : A -> Prep -> A2S ; --%
  mkAV  : A -> AV ; --%
---  mkA2V : A -> Prep -> A2V ; --%

-- Notice: categories $AS, A2S, AV, A2V$ are just $A$,
-- and the second argument is given
-- as an adverb. Likewise
-- $V0$ is just $V$.

  V0 : Type ; --%
  AS, A2S, AV, A2V : Type ; --%

--2 Structural categories

  mkAdV : Str -> AdV
   = \s -> lin AdV (ss s) ;
  mkAdA : Str -> AdA
   = \s -> lin AdA (ss s) ;
  mkAdN : Str -> AdN
   = \s -> lin AdN (ss s) ;
  mkPConj : Str -> PConj
   = \s -> lin PConj (ss s) ;
  mkSubj : Str -> Subj
   = \s -> lin Subj (ss s) ;

  mkPredet : Str -> Predet  -- invariable Predet, such as "vain"
   = \s -> lin Predet {s = \\_,_ => s} ;

  mkConj = overload {
   mkConj : Str -> Conj
   = \y -> {s1 = [] ; s2 = y ; n = Pl ; lock_Conj = <>} ;
   mkConj : Str -> Str -> Conj
   = \x,y -> {s1 = x ; s2 = y ; n = Pl ; lock_Conj = <>} ;
   mkConj : Str -> Str -> Number -> Conj
   = \x,y,n -> {s1 = x ; s2 = y ; n = n ; lock_Conj = <>} ;
   } ;

  mkDet = overload {
    mkDet : Number -> N -> Det
    = \nu,noun -> lin Det (MorphoFin.mkDet nu (snoun2nounBind noun)) ;
    mkDet : (isNeg : Bool) -> Number -> N -> Det  -- use this with True to create a negative determiner
    = \isNeg,nu,noun -> lin Det (MorphoFin.mkDetPol isNeg nu (snoun2nounBind noun)) ;
    mkDet : (isNeg : Bool) -> Number -> N -> Case -> Det  -- paljon + False + partitive, ei yhtään + True + partitive
    = \isNeg,nu,noun,c -> case c of {
      Part => lin Det (MorphoFin.mkDetPol isNeg nu (snoun2nounBind noun)) ** {isNum = True} ; --- works like "kolme autoa"
      _ => lin Det (MorphoFin.mkDetPol isNeg nu (snoun2nounBind noun)) ---- are there other cases?
      } ;
    } ;

  mkQuant = overload {
    mkQuant : N -> Quant =
      \noun -> heavyQuant {s1 = \\n,c => (snoun2nounBind noun).s ! NCase n c ; s2 = \\_ => [] ; isNum,isPoss,isNeg,isDef = False} ;
    mkQuant : N -> N -> Quant =
      \sg,pl -> heavyQuant {s1 = table {Sg => \\c => (snoun2nounBind pl).s ! NCase Sg c ; Pl => \\c => (snoun2nounBind pl).s ! NCase Pl c} ;
                            s2 = \\_ => [] ; isNum,isPoss,isNeg,isDef = False} ;
    } ;

  mkInterj : Str -> Interj
    = \s -> lin Interj {s = s} ;

--.
-- THE definitions should not bother the user of the API. So they are
-- hidden from the document.

  Case = MorphoFin.Case ;
  Number = MorphoFin.Number ;

  singular = Sg ;
  plural = Pl ;

  nominative = Nom ;
  genitive = Gen ;
  partitive = Part ;
  translative = Transl ;
  inessive = Iness ;
  essive  = Ess ;
  elative = Elat ;
  illative = Illat ;
  adessive = Adess ;
  ablative = Ablat ;
  allative = Allat ;

  infFirst = Inf1 ;
  infElat = Inf3Elat ; infIllat = Inf3Illat ;
  infIness = Inf3Iness ; infAdess = Inf3Adess ;
  infPart = Inf4Part ;
  infPresPart = InfPresPart ; infPresPartAgr = InfPresPartAgr ;

  prePrep  : Case -> Str -> Prep =
    \c,p -> lin Prep {c = NPCase c ; s = <tagFeature (tagPOS "ADP" p) "AdvType" "Pre", [],\\_ => []>} ; -- no possessive suffix

  postPrep : Case -> Str -> Prep =
    \c,p ->
       let
         h = guessHarmony p ;
         a2p : Agr => Str = case c of {
           Gen => \\a => p ++ possSuffixGen h a ;
           _ => \\a => p
           } ;
	 pt = tagFeature (tagPOS "ADP" p) "AdvType" "Post" ;
       in case p of {
         mukaa + "n" => lin Prep {c = NPCase c ; s = <[],pt, a2p>} ; ---- p --> mukaa
         _           => lin Prep {c = NPCase c ; s = <[],pt, a2p>}
         } ;

  postGenPrep = postPrep genitive ;

  casePrep : Case -> Prep =
    \c -> lin Prep {c = NPCase c ; s = <[],[],\\_ => []>} ;

  accPrep : Prep =
    lin Prep {c = NPAcc ; s = <[],[],\\_ => []>} ;

  NK = {s : NForms} ; --- lock_NK : {}} ;
  AK = {s : NForms} ; --- lock_AK : {}} ;
  VK = {s : VForms} ; --- lock_VK : {}} ;
  AdvK = {s : Str} ; --- lock_AdvK : {}} ;


  mkN = overload {
    mkN : (talo : Str) -> N = mk1N ;
    --  \s -> nforms2snoun (nForms1 s) ;
    mkN : (talo,talon : Str) -> N = mk2N ;
    --  \s,t -> nforms2snoun (nForms2 s t) ;
    mkN : (talo,talon,taloja : Str) -> N = mk3N ;
    --  \s,t,u -> nforms2snoun (nForms3 s t u) ;
    mkN : (talo,talon,taloja,taloa : Str) -> N = mk4N ;
    --  \s,t,u,v -> nforms2snoun (nForms4 s t u v) ;
    mkN :
      (talo,talon,taloa,talona,taloon,talojen,taloja,taloina,taloissa,taloihin
        : Str) -> N = mk10N ;
    mkN : (sora : Str) -> (tie : N) -> N = mkStrN ;
    mkN : (oma,tunto : N) -> N = mkNN ;
    mkN : (sana : NK) -> N = \w -> lin N (nforms2snoun w.s) ;
    mkN : V -> N = \w -> lin N (sverb2snoun w) ;
  } ;

    exceptNomN : N -> Str -> N = \noun,nom -> lin N (exceptNomSNoun noun nom) ;

----  mk1A : Str -> A = \jalo -> aForms2A (nforms2aforms (nForms1 jalo)) ;
----  mkNA : N -> A = snoun2sadj ;

  mk1N : (talo : Str) -> N = \s -> lin N (nforms2snoun (nForms1 s)) ;
  mk2N : (talo,talon : Str) -> N = \s,t -> lin N (nforms2snoun (nForms2 s t)) ;
  mk3N : (talo,talon,taloja : Str) -> N = \s,t,u -> lin N (nforms2snoun (nForms3 s t u)) ;
  mk4N : (talo,talon,taloa,taloja : Str) -> N = \s,t,u,v ->
      lin N (nforms2snoun (nForms4 s t u v)) ;
  mk10N :
      (talo,talon,taloa,talona,taloon,talojen,taloja,taloina,taloissa,taloihin
        : Str) -> N = \a,b,c,d,e,f,g,h,i,j ->
        lin N (nforms2snoun (nForms10 a b c d e f g h i j)) ;

  mkSeparateN : Str -> N -> N = \unissa,kulkija -> {
    s = \\c => unissa ++ kulkija.s ! c ;
    h = kulkija.h ;
    lock_N = <>
    } ;
  mkStrN : Str -> N -> N = \sora,tie -> {
    s = \\c => sora + tie.s ! c ;
    h = tie.h ;
    lock_N = <>
    } ;
  mkNN : N -> N -> N = \oma,tunto -> {
    s = \\c => oma.s ! c + tunto.s ! c ;
    h = tunto.h ;
    lock_N = <>
    } ; ---- TODO: oma in possessive suffix forms

  nForms1 : Str -> NForms = \ukko ->
    let
      ukk = init ukko ;
      uko = weakGrade ukko ;
      ukon = uko + "n" ;
      o = case last ukko of {"ä" => "ö" ; "a" => "o"} ; -- only used then
      renka = strongGrade (init ukko) ;
      rake = strongGrade ukko ;
    in
    case ukko of {
      _ + "nen" => dNainen ukko ;
      _ + ("aa" | "ee" | "ii" | "oo" | "uu" | "yy" |"ää"|"öö") => dPuu ukko ;
      _ + ("ai" | "ei" | "oi" | "ui" | "yi" | "äi" | "öi") => dPuu ukko ;
      _ + ("ie" | "uo" | "yö") => dSuo ukko ;
      _ + ("ea" | "eä") => dKorkea ukko ;
      _ + "is" => dKaunis ukko ;
      _ + ("ai" | "ui" | "äi") + "n" => dLiitin ukko (ukk + "men") ;
      _ + ("i" | "u") + "n" => dLiitin ukko (renka + "men") ;
      _ + ("ton" | "tön") => dOnneton ukko ;
      _ + "e" => dRae ukko (rake + "en") ;
      _ + ("ut" | "yt") => dOttanut ukko ;
      _ + ("as" | "äs") => dRae ukko (renka + last renka + "n") ;
      _ + ("uus" | "yys" | "eus" | "eys") => dLujuus ukko ;
      _ + "s" => dJalas ukko ;

-- {- heuristics for 3-syllable nouns ending a/ä
      _ + ("a" | "e" | "i" | "o" | "u" | "y" | "ä" | "ö") + ? +
          _ + "i" + ? + a@("a" | "ä") =>
          dSilakka ukko (ukko + "n") (ukk + o + "it" + a) ;  -- pesijä
      _ + ("a" | "e" | "i" | "o" | "u" | "y" | "ä" | "ö") + ? + _ +
          ("a" | "e" | "o" | "u" | "y" | "ä" | "ö") +
          ("l" | "r" | "n") + a@("a" | "ä") =>
          dSilakka ukko (ukko + "n") (ukk + o + "it" + a) ;  -- sarana, omena
      _ + ("a" | "e" | "i" | "o" | "u" | "y" | "ä" | "ö") + ? + _ +
          ("a" | "e" | "i" | "o" | "u" | "y" | "ä" | "ö") +
          ("n" | "k" | "s") + "k" + a@("a" | "ä") =>
          dSilakka ukko (uko + "n") (init uko + o + "it" + a) ;  -- silakka
      _ + ("a" | "e" | "i" | "o" | "u" | "y" | "ä" | "ö") + ? + _ +
          ("a" | "e" | "i" | "o" | "u" | "y" | "ä" | "ö") +
          ("n" | "t" | "s") + "t" + a@("a" | "ä") =>
          dSilakka ukko (uko + "n") (ukk + o + "j" + a) ;  -- yhdyntä (but not isäntä)
      _ + ("a" | "e" | "i" | "o" | "u") + ? + _ +
          ("a" | "e" | "o" | "u") + ? + "a" =>
          dSilakka ukko (ukko + "n") (ukk + "ia") ;  -- asema, johtaja
-- -}
      _ + "i" +o@("o"|"ö") => dSilakka ukko (ukko+"n") (ukko+"it"+getHarmony o);
      _ + "i" + "a" => dSilakka ukko (ukko + "n") (ukk + "oita") ;
      _ + "i" + "ä" => dSilakka ukko (ukko + "n") (ukk + "öitä") ;
      _ + ("a" | "o" | "u" | "y" | "ä" | "ö") => dUkko ukko ukon ;
      _ + "i" => dPaatti ukko ukon ;
      _ + ("ar" | "är") => dPiennar ukko (renka + "ren") ;
      _ + "e" + ("l" | "n") => dPiennar ukko (ukko + "en") ;
      _ => dUnix ukko
    } ;


    nForms2 : (_,_ : Str) -> NForms = \ukko,ukkoja ->
      let
        ukot = nForms1 ukko ;
        ukon = weakGrade ukko + "n" ;
      in
      case <ukko,ukkoja> of {
        <_, _ + ":" + ? + ("a" | "ä")> => dSDP ukko ;
        <_ + "ea", _ + "oita"> =>
          dSilakka ukko ukon ukkoja ;  -- idea, but not korkea
        <_ + ("aa" | "ee" | "ii" | "oo" | "uu" | "yy" | "ää" | "öö" |
              "ie" | "uo" | "yö" | "ea" | "eä" |
              "ia" | "iä" | "io" | "iö"), _ + ("a" | "ä")> =>
          nForms1 ukko ; --- to protect --- how to get "dioja"?
        <_ + ("a" | "ä" | "o" | "ö"), _ + ("a" | "ä")> =>
          dSilakka ukko ukon ukkoja ;
        <arp + "i", _ + "i" + ("a" | "ä")> =>
          dArpi ukko (init (weakGrade ukko) + "en") ;
        <_ + "i", _ + ("eita" | "eitä")> =>
          dTohtori ukko ;
        <_ + ("ut" | "yt"),_ + ("uita" | "yitä")>  => dRae ukko (init ukko + "en") ;
        <_ + "e", nuk + ("eja" | "ejä")> =>
          dNukke ukko ukon ;
        <_ + "s", _ + "ksi" + ?> => dJalas ukko ;
        <_ + ("l" | "n" | "r" | "s"), _ + ("eja" | "ejä")> => dUnix ukko ;
        <_, _ + ("a" | "ä")> => ukot ;
        _ =>
          Predef.error
           (["last argument should end in a/ä, not"] ++ ukkoja)
      } ;

    nForms3 : (_,_,_ : Str) -> NForms = \ukko,ukon,ukkoja ->
      let
        ukk = init ukko ;
        ukot = nForms2 ukko ukkoja ;
      in
      case <ukko,ukon,ukkoja> of {
        <muk + "i", muk_ + "in",
         muk__ + "ej" + ("a"|"ä")> -- don't match voi - voin - voita
           => dPaatti ukko ukon ;  -- 1-arg paradigm forces consonant gradation
        <_, _ + ":n"> => dSDP ukko ;
        <_ + ("aa" | "ee" | "ii" | "oo" | "uu" | "yy" | "ää" | "öö" |
              "ie" | "uo" | "yö" | "ea" | "eä" |
              "ia" | "iä" | "io" | "iö" | "ja" | "jä"), _ + "n"> =>
           ukot ; --- to protect
        <_ + ("a" | "o" | "u" | "y" | "ä" | "ö"), _ + "n"> =>
          dSilakka ukko ukon ukkoja ;  -- auto,auton
        <_ + "mpi", _ + ("emman" | "emmän")> => dSuurempi ukko ;
        <_ + "in", _ + ("imman" | "immän")> => dSuurin ukko ;
        <lämm + "in", lämpi + ("man"|"män")> => dLämmin ukko ukon ;
        <terv + "e", terv2 + "een"> => -- was nonlinear
          dRae ukko ukon ;
        <taiv + ("as" | "äs"), taiv2 + ("aan" | "ään")> =>  -- was nonlinear
          dRae ukko ukon ;
        <nukk + "e", nuk + "een"> => dRae ukko ukon ;
        <arp + "i", arv + "en"> => dArpi ukko ukon ;
        <_ + ("us" | "ys"), _ + "den"> => dLujuus ukko ;
        <laid + ("u"|"a"|"ä") + "n", laitu + "men">
          => dLiitin ukko ukon ; -- laidun,hapan,sydän not caught in previous
        <_, _ + "n"> => ukot ;
        _ =>
          Predef.error (["second argument should end in n, not"] ++ ukon)
       } ;

    nForms4 : (_,_,_,_ : Str) -> NForms = \ukko,ukon,ukkoja,ukkoa ->
      let
        ukot = nForms3 ukko ukon ukkoja ;
      in
      case <ukko,ukon,ukkoja,ukkoa> of {
        <_,_ + "n", _ + ("a" | "ä"), _ + ("a" | "ä")> =>
          table {
            2 => ukkoa ;
            n => ukot ! n
          } ;
        _ =>
          Predef.error
            (["last arguments should end in n, a/ä, and a/ä, not"] ++
            ukon ++ ukkoja ++ ukkoa)
      } ;

  mkN2 = overload {
    mkN2 : N -> N2 = \n -> mmkN2 n (casePrep genitive) ;
    mkN2 : N -> Prep -> N2 = mmkN2
    } ;

  mmkN2 : N -> Prep -> N2 = \n,c -> n ** {c2 = c ; isPre = mkIsPre c ; lock_N2 = <> ; postmod = \\_ => []} ;
  mkN3 = \n,c,e -> n ** {c2 = c ; c3 = e ;
    isPre = mkIsPre c  ; -- matka Lontoosta Pariisiin
    isPre2 = mkIsPre e ;          -- Suomen voitto Ruotsista
    lock_N3 = <>
    } ;

  mkIsPre : Prep -> Bool = \p -> case p.c of {
    NPCase Gen => case p.s.p2 of {
      "" => False ;      -- Jussin veli
      _  => True         -- puhe Jussin puolesta
      } ;
    _ => True            -- syyte Jussia vastaan ; puhe Jussille
    } ;

  mkPN = overload {
    mkPN : Str -> PN = mkPN_1 ;
    mkPN : N -> PN = \s -> lin PN (snoun2spn s) ;
    } ;

  mkPN_1 : Str -> PN = \s -> lin PN (snoun2spn (mk1N s)) ;

  foreignPN : Str -> PN = \s -> (lin PN (snoun2spn (nforms2snoun (noun s)))) where {
    noun : Str -> NForms = \s -> case s of {
      _ + "i" => dPaatti s (s + "n") ;
      _ + "e" => dNukke s (s + "n") ;
      _ + ("a" | "o" | "u" | "y" | "ä" | "ö" | "ü") => dUkko s (s + "n") ;
      _ => dUnix s
      }
    } ;


-- adjectives

  mkA = overload {
    mkA : Str -> A  = mkA_1 ;
    mkA : N -> A = \n -> noun2adjDeg n ** {lock_A = <>} ;
    mkA : N -> (kivempi,kivin : Str) -> A = \n -> regAdjective n ;
    mkA : (sana : AK) -> A = \w -> noun2adjDeg (nforms2snoun w.s) ;

    mkA : (hyva,parempi,paras : N) -> (hyvin,paremmin,parhaiten : Str) -> A
      = \h,p,ps,hn,pn,ph -> lin A (mkAdj h p ps hn pn ph ** {p=[]; hasPrefix=False}) ;
    mkA : V -> A = presActA ;
    } ;

  mkA_1 : Str -> A = \x -> lin A (noun2adjDeg (mk1N x)) ;

-- auxiliaries
  mkAdjective : (_,_,_ : SAdj) -> A = \hyva,parempi,paras -> lin A
    {s = table {
      Posit  => hyva.s ;
      Compar => parempi.s ;
      Superl => paras.s
      } ;
     h = hyva.h ;  ---- different for parempi, paras
     p = [] ;
     hasPrefix = False
    } ;
  regAdjective : SNoun -> Str -> Str -> A = \kiva, kivempi, kivin ->
    mkAdjective
      (snoun2sadj kiva)
      (snoun2sadjComp False (nforms2snoun (dSuurempi kivempi)))
      (snoun2sadjComp False (nforms2snoun (dSuurin kivin))) ;
  noun2adjDeg : SNoun -> A = \suuri ->
    regAdjective
      suuri
      (snoun2compar suuri)
      (snoun2superl suuri) ;


  presActA : SVerb -> A = \tulla ->
    let tuleva : NForm => Str = \\nf => (sverb2verb True tulla).s ! PresPartAct (AN nf) ;
     in noun2adjDeg { s = tuleva ; h = tulla.h } ;

  presPassA : SVerb -> A = \mennä ->
    let mentävä : NForm => Str = \\nf => (sverb2verb True mennä).s ! PresPartPass (AN nf) ;
     in noun2adjDeg { s = mentävä ; h = mennä.h } ;

  pastActA : SVerb -> A = \syntyä ->
    let syntynyt : NForm => Str = \\nf => (sverb2verb True syntyä).s ! PastPartAct (AN nf) ;
     in noun2adjDeg  { s = syntynyt ; h = syntyä.h } ;

  pastPassA : SVerb -> A = \sulkea ->
    let suljettu : NForm => Str = \\nf => (sverb2verb True sulkea).s ! PastPartPass (AN nf) ;
     in noun2adjDeg  { s = suljettu ; h = sulkea.h } ;

-- verbs

  mkV = overload {
    mkV : (huutaa : Str) -> V = mk1V ;
    mkV : (huutaa,huusi : Str) -> V = mk2V ;
    mkV : (huutaa,huudan,huusi : Str) -> V = mk3V ;
    mkV : (
      huutaa,huudan,huutaa,huutavat,huutakaa,huudetaan,
      huusin,huusi,huusisi,huutanut,huudettu,huutanee : Str) -> V = mk12V ;
    mkV : (sana : VK) -> V = \w -> vforms2sverb w.s ** {sc = SCNom ; lock_V = <> ; p = []} ;
    mkV : V -> Str -> V = \w,p -> {s = w.s ; sc = w.sc ; lock_V = <> ; h = w.h ; p = p} ;
    mkV : Str -> V -> V = \s,v -> {s = \\f => s + v.s ! f ; sc = v.sc ; lock_V = <> ; h = v.h ; p = v.p} ;
  } ;

  mk1V : Str -> V = \s ->
    let vfs = vforms2sverb (vForms1 s) in
      vfs ** {sc = SCNom ; lock_V = <> ; p = []} ;
  mk2V : (_,_ : Str) -> V = \x,y ->
    let vfs = vforms2sverb (vForms2 x y) in vfs ** {sc = SCNom ; lock_V = <> ; p = []} ;
  mk3V : (huutaa,huudan,huusi : Str) -> V = \x,_,y -> mk2V x y ; ----
  mk12V : (
      huutaa,huudan,huutaa,huutavat,huutakaa,huudetaan,
      huusin,huusi,huusisi,huutanut,huudettu,huutanee : Str) -> V =
     \a,b,c,d,e,f,g,h,i,j,k,l ->
        vforms2sverb (vForms12 a b c d e f g h i j k l) ** {sc = SCNom ; lock_V = <> ; p = []} ;

  vForms1 : Str -> VForms = \ottaa ->
    let
      a = last ottaa ;
      otta = init ottaa ;
      ott  = init otta ;
      ots  = init ott + "s" ;
      ota  = weakGrade otta ;
      otin = init (strongGrade (init ott)) + "elin" ;
      ot   = init ota ;
    in
    case ottaa of {
      _ + ("e" | "i" | "o" | "u" | "y" | "ö") + ("a" | "ä") =>
        cHukkua ottaa (ota + "n") ;
      _ + ("l" | "n" | "r") + ("taa" | "tää") =>
        cOttaa ottaa (ota + "n") (ots + "in") (ots + "i") ;
      ("" | ?) + ("a" | "e" | "i" | "o" | "u") + ? + _ +
        ("a" | "e" | "i" | "o" | "u") + _ + "aa" =>
        cOttaa ottaa (ota + "n") (ot + "in") (ott + "i") ;
      ("" | ?) + ("a" | "e" | "i") + _ + "aa" =>
        cOttaa ottaa (ota + "n") (ot + "oin") (ott + "oi") ;
      _ + ("aa" | "ää") =>
        cOttaa ottaa (ota + "n") (ot + "in") (ott + "i") ;
      _ + ("ella" | "ellä") =>
        cKuunnella ottaa otin ;
      _ + ("osta" | "östä") =>
        cJuosta ottaa (init ott + "ksen") ;
      _ + ("st" | "nn" | "ll" | "rr") + ("a" | "ä") =>
        cJuosta ottaa (ott + "en") ;
      _ + ("ita" | "itä") =>
        cHarkita ottaa ;
      _ + ("eta" | "etä" | "ota" | "ata" | "uta" | "ytä" | "ätä" | "ötä") =>
        cPudota ottaa (strongGrade ott + "si") ;
      _ + ("da" | "dä") =>
        cJuoda ottaa ;
      _ => Predef.error (["expected infinitive, found"] ++ ottaa)
    } ;

  vForms2 : (_,_ : Str) -> VForms = \huutaa,huusi ->
    let
      huuda = weakGrade (init huutaa) ;
      huusin = weakGrade huusi + "n" ;
      autoin = weakGrade (init huusi) + "in" ;
    in
    case <huutaa,huusi> of {
      <_ + ("taa" | "tää"), _ + ("oi" | "öi")> =>
        cOttaa huutaa (huuda + "n") autoin huusi ;
      <_ + ("aa" | "ää"), _ + "i"> =>
        cOttaa huutaa (huuda + "n") huusin huusi ;
      <_ + ("eta" | "etä"), _ + "eni"> =>
        cValjeta huutaa huusi ;
      <_ + ("sta" | "stä"), _ + "si"> =>
        vForms1 huutaa ; -- pestä, halkaista
      <_ + ("ta" | "tä"), _ + "si"> =>
        cPudota huutaa huusi ;
      <_ + ("lla" | "llä"), _ + "li"> =>
        cKuunnella huutaa huusin ;
      _ => vForms1 huutaa
      } ;



  caseV c v = {s = v.s ; sc = npform2subjcase (NPCase c) ; h = v.h ; lock_V = <> ; p = v.p} ;

  vOlla = {
    s = ollaSVerbForms ;
    sc = SCNom ; h = Back ; lock_V = <> ; p = []} ; ---- lieneekö

  mk2V2 : V -> Prep -> V2 = \v,c -> v ** {c2 = c ; lock_V2 = <>} ;
  caseV2 : V -> Case -> V2 = \v,c -> mk2V2 v (casePrep c) ;
  dirV2 v = mk2V2 v accPrep ;

  mkAdv = overload {
    mkAdv : Str -> Adv = \s -> {s = tagPOS "ADV" s ; lock_Adv = <>} ;
    mkAdv : AdvK -> Adv = \s -> {s = tagPOS "ADV" s.s ; lock_Adv = <>} ;
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2 = \s -> dirV2 (mk1V s) ;
    mkV2 : Str -> Case -> V2 = \s -> caseV2 (mk1V s) ;
    mkV2 : V -> V2 = dirV2 ;
    mkV2 : V -> Case -> V2 = caseV2 ;
    mkV2 : V -> Prep -> V2 = mk2V2 ;
    mkV2 : VK -> V2 = \w -> dirV2 (vforms2sverb w.s ** {sc = SCNom ; lock_V = <> ; p = []}) ;
    } ;

  mk2V2 : V -> Prep -> V2 ;
  caseV2 : V -> Case -> V2 ;
  dirV2 : V -> V2 ;

  dirV3 v p = v ** {c2 = accPrep ; c3 = casePrep p ; lock_V3 = <>} ;
  dirdirV3 v = dirV3 v allative ;


  mkVVf  v f = v ** {vi = infform2vvtype f ; lock_VV = <>} ;
  mkVQ  v = v ** {lock_VQ = <>} ;

  V0 : Type = V ;
  AS, A2S, AV : Type = A ;
  A2V : Type = A2 ;

  mkV0  v = v ** {lock_V = <>} ;
  mkV2Sbare : V -> V2S = \v -> mkV2S v (casePrep allative) ; ----

  mkV2S v p = mk2V2 v p ** {lock_V2S = <>} ;
  mkV2Vbare : V -> V2V = \v -> mkV2Vf v (casePrep partitive) infIllat ; ----
--  mkV2V v p = mkV2Vf v p infIllat ;
  mkV2Vf v p f = mk2V2 v p ** {vi = infform2vvtype f ; lock_V2V = <>} ;

  mkVAbare : V -> VA = \v -> mkVA v (casePrep partitive) ; ----
  mkVA  v p = v ** {c2 = p ; lock_VA = <>} ;
  mkV2Abare : V -> V2A = \v -> mkV2A v (casePrep partitive) (casePrep translative) ;
  mkV2A v p q = v ** {c2 = p ; c3 = q ; lock_V2A = <>} ;
  mkV2Qbare : V -> V2Q = \v -> mkV2Q v (casePrep ablative) ; ----
  mkV2Q v p = mk2V2 v p ** {lock_V2Q = <>} ;

  mkAS  v = v ** {lock_A = <>} ;
---  mkA2S v p = mkA2 <v : A> p ** {lock_A = <>} ;
  mkAV  v = v ** {lock_A = <>} ;
---  mkA2V v p = mkA2 <v : A> p ** {lock_A2 = <>} ;

} ;
