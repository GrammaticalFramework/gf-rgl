--# -path=.:../romance:../common:../abstract:../../prelude

--1 Italian Lexical Paradigms
--
-- Aarne Ranta 2003
--
-- This is an API for the user of the resource grammar 
-- for adding lexical items. It gives functions for forming
-- expressions of open categories: nouns, adjectives, verbs.
-- 
-- Closed categories (determiners, pronouns, conjunctions) are
-- accessed through the resource syntax API, $Structural.gf$. 
--
-- The main difference with $MorphoIta.gf$ is that the types
-- referred to are compiled resource grammar types. We have moreover
-- had the design principle of always having existing forms, rather
-- than stems, as string arguments of the paradigms.
--
-- The structure of functions for each word class $C$ is the following:
-- first we give a handful of patterns that aim to cover all
-- regular cases. Then we give a worst-case function $mkC$, which serves as an
-- escape to construct the most irregular words of type $C$.
-- However, this function should only seldom be needed. For verbs, we have a
-- separate module [``BeschIta`` ../../italian/BeschIta.gf],
-- which covers the "Bescherelle" verb conjugations.

resource ParadigmsIta = BeschIta **
  open 
    (Predef=Predef), 
    Prelude, 
    MorphoIta, 
--    BeschIta,
    CatIta in {

  flags optimize=all ;
    coding=utf8 ;

--2 Parameters 
--
-- To abstract over gender names, we define the following identifiers.

oper
  Gender : Type ; 

  masculine : Gender ;
  feminine  : Gender ;

-- To abstract over number names, we define the following.

  Number : Type ; 

  singular : Number ;
  plural   : Number ;

-- Prepositions used in many-argument functions are either strings
-- (including the 'accusative' empty string) or strings that
-- amalgamate with the following word (the 'genitive' "di" and the
-- 'dative' "a").

  --Prep : Type ;

  accusative : Prep ; -- direct object
  genitive   : Prep ; -- preposition "di" and its contractions
  dative     : Prep ; -- preposition "a" and its contractions

  di_Prep    : Prep ; -- = genitive
  a_Prep     : Prep ; -- = dative

-- The following prepositions also have special contracted forms.

  con_Prep : Prep ; -- preposition "con" and its contractions
  da_Prep : Prep ; -- preposition "da" and its contractions
  in_Prep : Prep ; -- preposition "in" and its contractions
  su_Prep : Prep ; -- preposition "su" and its contractions

-- other prepositions

  mkPrep : overload {
    mkPrep : Str -> Prep  ;         -- simple preposition (other than a, di, con, da, in, su)
    mkPrep : Str -> Prep -> Prep ;  -- complex preposition e.g. "vicino a"
    } ;


--2 Nouns

  mkN : overload {

-- The regular function takes the singular form and the gender,
-- and computes the plural and the gender by a heuristic. 
-- The heuristic says that the gender is feminine for nouns
-- ending with "a", and masculine for all other words.

    mkN : (cane : Str) -> N ; -- regular noun; fem for -"a", masc otherwise

-- To force a different gender, give it explicitly.

    mkN : (carne : Str) -> Gender -> N ; -- override default gender

-- Worst case: give both two forms and the gender. 

    mkN : (uomo,uomini : Str) -> Gender -> N ; -- worst case

-- In *compound nouns*, the first part is inflected as a noun but
-- the second part is not inflected. e.g. "numero di telefono". 
-- They could be formed in syntax, but we give a shortcut here since
-- they are frequent in lexica.

    mkN : N -> Str -> N -- compound such as "numero" + "di telefono"
    } ;




--3 Relational nouns 
-- 
-- Relational nouns ("figlio di x") need a case and a preposition. 
-- The default is regular nouns with the genitive "di".

  mkN2 : overload {
    mkN2 : Str -> N2 ; -- regular with genitive, e.g. "figlio" + "di"
    mkN2 : N -> Prep -> N2 -- arbitrary noun and preposition
  } ;

-- Three-place relational nouns ("la connessione di x a y") need two prepositions.

  mkN3 : N -> Prep -> Prep -> N3 ; -- e.g. volo + da + per


--3 Relational common noun phrases
--
-- In some cases, you may want to make a complex $CN$ into a
-- relational noun (e.g. "la vecchia chiesa di"). However, $N2$ and
-- $N3$ are purely lexical categories. But you can use the $AdvCN$
-- and $PrepNP$ constructions to build phrases like this.

-- 
--3 Proper names and noun phrases
--
-- Proper names need a string and a gender. The gender is by default feminine if
-- the name ends with an "a", and masculine otherwise.

  mkPN : overload {
    mkPN : Str -> PN ; -- femininne for "-a", otherwise masculine
    mkPN : Str -> Gender -> PN ; -- set gender manually
    mkPN : N -> PN ;  -- get gender from noun
  } ;



--2 Adjectives

  mkA : overload {

-- For regular adjectives, all forms are derived from the
-- masculine singular. Comparison is formed by "più".

    mkA : (bianco : Str) -> A ; -- predictable adjective

-- Five forms are needed in the worst
-- case (masc and fem singular, masc plural, adverbial), given that
-- comparison is formed by "più".

    mkA : (solo,sola,soli,sole,solamente : Str) -> A ; -- irregular adjective

-- With irregular comparison, there are as it were two adjectives:
-- the positive ("buono") and the comparative ("migliore"). 

    mkA : A -> A -> A ; -- special comparison, e.g. buono - migliore

    mkA : A -> CopulaType -> A -- force copula type
    } ;

-- All the functions above create postfix adjectives. To switch
-- them to prefix ones (i.e. ones placed before the noun in
-- modification, as in "vecchia chiesa"), the following function is
-- provided.

    prefixA : A -> A -- adjective that comes before noun (default: after) 
    = prefA ;



--3 Two-place adjectives
--
-- Two-place adjectives need a preposition for their second argument.

  mkA2 : A -> Prep -> A2 ; -- e.g. divisibile + per



--2 Adverbs

-- Adverbs are not inflected. Most lexical ones have position
-- after the verb. 

  mkAdv : Str -> Adv ;

-- Some appear next to the verb (e.g. "sempre").

  mkAdV : Str -> AdV ;

-- Adverbs modifying adjectives and sentences can also be formed.

  mkAdA : Str -> AdA ;


--2 Verbs

  mkV : overload {

-- Regular verbs are ones with the infinitive "are" or "ire", the
-- latter with singular present indicative forms as "finisco".
-- The regular verb function is the first conjugation recognizes
-- these endings, as well as the variations among
-- "amare, cominciare, mangiare, legare, cercare".

    mkV : Str -> V ; -- regular verbs in "-are"/"-ire"
 
-- The module $BeschIta$ gives (almost) all the patterns of the "Bescherelle"
-- book. To use them in the category $V$, wrap them with the function

    mkV : Verbo -> V ; -- verbs formed by BeschIta

-- If $BeschIta$ does not give the desired result or feels difficult
-- to consult, here is a worst-case function for "-ire" and "-ere" verbs,
-- taking 11 arguments.

    mkV : (udire,odo,ode,udiamo,udiro,udii,udisti,udi,udirono,odi,udito : Str) -> V ; -- worst case

    mkV : V -> Str -> V ; -- particle verb
    } ;

-- The function $regV$ gives all verbs the compound auxiliary "avere".
-- To change it to "essere", use the following function.
-- Reflexive implies "essere".

  essereV : V -> V ; -- force "essere" as auxiliary (default avere)
  reflV : V -> V ; -- reflexive verb (implies essere)


--3 Two-place verbs
--
-- Two-place verbs need a preposition, except the special case with direct object.
-- (transitive verbs). Notice that a particle comes from the $V$.

  mkV2 : overload {
    mkV2 : Str -> V2 ; -- regular verb, direct object 
    mkV2 : V -> V2 ;   -- direct object
    mkV2 : V -> Prep -> V2 -- prepositional object
    } ;

-- You can reuse a $V2$ verb in $V$.

  v2V : V2 -> V ; --%

--3 Three-place verbs
--
-- Three-place (ditransitive) verbs need two prepositions, of which
-- the first one or both can be absent.

  mkV3 : overload {
    mkV3 : V -> V3 ;                -- donner (+ accusative + dative)    
    mkV3 : V -> Prep -> V3 ;        -- placer (+ accusative) + dans
    mkV3 : V -> Prep -> Prep -> V3  -- parler + dative + genitive
    } ;

  dirV3    : V -> Prep -> V3 ;         -- dare,_,a
  dirdirV3 : V -> V3 ;                 -- dare,_,_


--3 Other complement patterns
--
-- Verbs and adjectives can take complements such as sentences,
-- questions, verb phrases, and adjectives.

  mkV0  : V -> V0 ; --%
  mkVS  : V -> VS ;
  subjVS  : V -> VS ;
  mkVV  : V -> VV ;  -- plain infinitive: "voglio parlare"
  deVV  : V -> VV ;  -- "cerco di parlare"
  aVV   : V -> VV ;  -- "arrivo a parlare"
  mkVA  : V -> VA ;
  mkVQ  : V -> VQ ;
  mkV2Q : V -> Prep -> V2Q ;
  mkV2S  : overload {
    mkV2S : V -> V2S ;
    mkV2S : V -> Prep -> V2S ;
    } ;
  mkV2V  : overload {
    mkV2V : V -> V2V ;
    mkV2V : V -> Prep -> Prep -> V2V ;
    } ;
  mkV2A : overload {
    mkV2A : V -> V2A ;
    mkV2A : V -> Prep -> Prep -> V2A ;
    } ;



  mkAS  : A -> AS ; --%
  mkA2S : A -> Prep -> A2S ; --%
  mkAV  : A -> Prep -> AV ; --%
  mkA2V : A -> Prep -> Prep -> A2V ; --%

-- Notice: categories $AS, A2S, AV, A2V$ are just $A$, 
-- and the second argument is given
-- as an adverb. Likewise 
-- $V0$ is just $V$.

  V0 : Type ; --%
  AS, A2S, AV, A2V : Type ; --%


--2 Closed categories

  mkPredet : Str -> Predet 
    = \s -> lin Predet {s = \\a,c => prepCase c ++ s ; c = Nom ; a = PNoAg} ;

--.
--2 The definitions of the paradigms
--
-- The definitions should not bother the user of the API. So they are
-- hidden from the document.


  Gender = MorphoIta.Gender ; 
  Number = MorphoIta.Number ;
  masculine = Masc ;
  feminine = Fem ;
  singular = Sg ;
  plural = Pl ;

  --Prep = Compl ;
  accusative = lin Prep complAcc ;
  genitive, di_Prep = lin Prep complGen ;
  dative, a_Prep = lin Prep complDat ;

  mkPrep = overload {
    mkPrep : Str -> Prep  = \p -> {s = p ; c = Acc ; isDir = False ; lock_Prep = <>} ;
    mkPrep : Str -> Prep -> Prep = \s,c-> {s = s ; c = c.c ; isDir = False ; lock_Prep = <>}
    } ;

  con_Prep = {s = [] ; c = CPrep P_con ; isDir = False ; lock_Prep = <>} ;
  da_Prep = {s = [] ; c = CPrep P_da ; isDir = False ; lock_Prep = <>} ;
  in_Prep = {s = [] ; c = CPrep P_in ; isDir = False ; lock_Prep = <>} ;
  su_Prep = {s = [] ; c = CPrep P_su ; isDir = False ; lock_Prep = <>} ;

  mk2N x y g = mkNounIrreg x y g ** {lock_N = <>} ;
  regN x = mkNomReg x ** {lock_N = <>} ;
  compN x y = {s = \\n => x.s ! n ++ y ; g = x.g ; lock_N = <>} ;
  femN x = {s = x.s ; g = feminine ; lock_N = <>} ;
  mascN x = {s = x.s ; g = masculine ; lock_N = <>} ;


  mk2N2 = \n,p -> n ** {lock_N2 = <> ; c2 = p} ;
  diN2 n = mk2N2 n genitive ;
  aN2 n = mk2N2 n dative ;
  mkN3 = \n,p,q -> n ** {lock_N3 = <> ; c2 = p ; c3 = q} ;

  mk2PN : Str -> Gender -> PN ;
  mk2PN x g = {s = x ; g = g} ** {lock_PN = <>} ;
  regPN x = mk2PN x g where {
    g = case last x of {
      "a" => feminine ;
      _ => masculine
      }
    } ;

  makeNP x g n = {s = (pn2np (mk2PN x g)).s; a = agrP3 g n ; hasClit = False ; isPol = False ; isNeg = False} ** {lock_NP = <>} ;

  mk5A a b c d e = 
   compADeg {s = \\_ => (mkAdj a b c d e).s ; isPre = False ; copTyp = serCopula ; lock_A = <>} ;
  regA a = compADeg {s = \\_ => (mkAdjReg a).s ; isPre = False ; copTyp = serCopula ; lock_A = <>} ;
  prefA a = {s = a.s ; isPre = True ; copTyp = a.copTyp ; lock_A = <>} ;
  adjCopula a cop = a ** {copTyp = cop} ;

  mkA2 a p = a ** {c2 = p ; lock_A2 = <>} ;

  mkADeg a b = 
   {s = table {Posit => a.s ! Posit ; _ => b.s ! Posit} ; 
    isPre = a.isPre ; copTyp = serCopula ; lock_A = <>} ;
  compADeg a = 
    {s = table {Posit => a.s ! Posit ; _ => \\f => "più" ++ a.s ! Posit ! f} ; 
     isPre = a.isPre ; copTyp = a.copTyp ;
     lock_A = <>} ;
  regADeg a = compADeg (regA a) ;

  mkAdv x = ss x ** {lock_Adv = <>} ;
  mkAdV x = ss x ** {lock_AdV = <>} ;
  mkAdA x = ss x ** {lock_AdA = <>} ;

  regV x = 
    let 
      are = Predef.dp 3 x ;
      ci  = Predef.dp 2 (Predef.tk 3 x) ;
      i   = last ci ;
      verb = case are of {
        "ire" =>  finire_100 x ;
        "ere" =>  temere_20 x ;
        "are" => case i of {
          "c" => cercare_7 x ;
          "g" => legare_8 x ;
          _ => case ci of {
            "ci" => cominciare_9 x ;
            "gi" => mangiare_10 x ;
           _    => amare_6 x
            }
          } ;
         _ => Predef.error ("regular verb infinitive must end are/ire/ere, not satisfied by" ++ x)
        }
    in verbBesch verb ** {vtyp = VHabere ; p = [] ; lock_V = <>} ;

  verboV ve = verbBesch ve ** {vtyp = VHabere ; p = [] ; lock_V = <>} ;

  mk11V 
    dovere devo deve dobbiamo dovro 
    dovetti dovesti dovette dovettero dovi dovuto = verboV (mkVerbo
      dovere devo deve dobbiamo dovro 
      dovetti dovesti dovette dovettero dovi dovuto
      ) ;

  essereV v = v ** {vtyp = VEsse} ;
  reflV v = v ** {vtyp = VRefl} ;

  mk2V2 v p = lin V2 (v ** {c2 = p}) ;
  dirV2 v = mk2V2 v accusative ;
  v2V v = v ** {lock_V = <>} ;


  V0 : Type = V ;
  AS, AV : Type = A ;
  A2S, A2V : Type = A2 ;

  mkV0  v = v ** {lock_V0 = <>} ;
  mkVS  v = v ** {m = \\_ => Indic ; lock_VS = <>} ;  ---- more moods
  subjVS  v = v ** {m = \\_ => Conjunct ; lock_VS = <>} ;
  mkVV  v = v ** {c2 = complAcc ; lock_VV = <>} ;
  deVV  v = v ** {c2 = complGen ; lock_VV = <>} ;
  aVV  v = v ** {c2 = complDat ; lock_VV = <>} ;
  mkVA  v = v ** {lock_VA = <>} ;
  mkVQ  v = v ** {lock_VQ = <>} ;
  mkV2Q v p = mk2V2 v p ** {lock_V2Q = <>} ;

  mmkV3    : V -> Prep -> Prep -> V3 ;  -- parler, à, de
  mmkV3 v p q = v ** {c2 = p ; c3 = q ; lock_V3 = <>} ;
  dirV3 v p = mmkV3 v accusative p ;
  dirdirV3 v = mmkV3 v dative accusative ;

  mmkV2 : V -> Prep -> V2 ;
  mmkV2 v p = v ** {c2 = p ; lock_V2 = <>} ;

  mkV3 = overload {
    mkV3 : V -> V3 = dirdirV3 ;               -- donner,_,_
    mkV3 : V -> Prep -> V3 = dirV3 ;          -- placer,_,sur
    mkV3 : V -> Prep -> Prep -> V3 = mmkV3    -- parler, à, de
    } ;

  mkV2S = overload {
    mkV2S : V -> V2S = \v -> mmkV2 v dative ** {mn,mp = Indic ; lock_V2S = <>} ;
    mkV2S : V -> Prep -> V2S = \v,p -> mmkV2 v p ** {mn,mp = Indic ; lock_V2S = <>} ;
    } ;
  mkV2V = overload {
    mkV2V : V -> V2V                 = \v -> mmkV3 v accusative dative ** {lock_V2V = <>} ;
    mkV2V : V -> Prep -> Prep -> V2V = \v,p,q -> mmkV3 v p q ** {lock_V2V = <>} ;
    } ;

  mkV2A = overload {
    mkV2A : V -> V2A                 = \v -> mmkV3 v accusative dative ** {lock_V2A = <>} ;
    mkV2A : V -> Prep -> Prep -> V2A = \v,p,q -> mmkV3 v p q ** {lock_V2A = <>} ;
    } ;


  mkAS  v = v ** {lock_AS = <>} ; ---- more moods
  mkA2S v p = mkA2 v p ** {lock_A2S = <>} ;
  mkAV  v p = v ** {c = p.p1 ; s2 = p.p2 ; lock_AV = <>} ;
  mkA2V v p q = mkA2 v p ** {s3 = q.p2 ; c3 = q.p1 ; lock_A2V = <>} ;

------------------------

  mkN = overload {
    mkN : (cane : Str) -> N = regN ;
    mkN : (carne : Str) -> Gender -> N = \n,g -> {s = (regN n).s ; g = g ; lock_N = <>} ;
    mkN : (uomo,uomini : Str) -> Gender -> N = mk2N ;
    mkN : N -> Str -> N = compN
    } ;

  compN : N -> Str -> N ;
  mk2N  :(uomo,uomini : Str) -> Gender -> N ;
  regN  : Str -> N ;
  mascN : N -> N ;
  femN  : N -> N ;

  mkN2 = overload {
    mkN2 : Str -> N2 = \s -> diN2 (regN s) ;
    mkN2 : N -> Prep -> N2 = mk2N2
  } ;

  mk2N2 : N -> Prep -> N2 ;
  diN2 : N -> N2 ;
  aN2  : N -> N2 ;

  regPN : Str -> PN ;           -- feminine if "-a", otherwise masculine

-- obsolete
  makeNP : Str -> Gender -> Number -> NP ; 

  mkPN = overload {
    mkPN : Str -> PN = regPN ;
    mkPN : Str -> Gender -> PN = mk2PN ;
    mkPN : N -> PN = \n -> lin PN {s = n.s ! Sg ; g = n.g} ;
  } ;

  mkA = overload {
    mkA : (bianco : Str) -> A = regA ;
    mkA : (solo,sola,soli,sole, solamente : Str) -> A = mk5A ;
    mkA : A -> A -> A = mkADeg ;
    mkA : A -> CopulaType -> A = adjCopula ;
    } ;
  mk5A : (solo,sola,soli,sole, solamente : Str) -> A ;
  regA : Str -> A ;
  prefA : A -> A ;
  adjCopula : A -> CopulaType -> A ;
  mkADeg : A -> A -> A ;
  compADeg : A -> A ;
  regADeg : Str -> A ;

  mkV = overload {
    mkV : Str -> V = \s -> case s of {
      _ + "rsi" => reflV (regV (Predef.tk 2 s + "e")) ;
      _ => regV s
      } ;
    mkV : Verbo -> V = verboV ;
    mkV : 
     (udire,odo,ode,udiamo,udiro,udii,udisti,udi,udirono,odi,udito : Str) -> V = mk11V ; 

    mkV : V -> Str -> V = \v,p -> v ** {p = p} ;  ---- to recognize particles in dict, not yet in lincat V
    } ;

  regV : Str -> V ;
  verboV : Verbo -> V ;
  mk11V : 
    (udire,odo,ode,udiamo,udiro,udii,udisti,udi,udirono,odi,udito : Str) -> V ; 

  mkV2 = overload {
    mkV2 : Str -> V2 = \s -> dirV2 (regV s) ;
    mkV2 : V -> V2 = dirV2 ;
    mkV2 : V -> Prep -> V2 = mk2V2
    } ;
  mk2V2  : V -> Prep -> V2 ;
  dirV2 : V -> V2 ;

} ;
