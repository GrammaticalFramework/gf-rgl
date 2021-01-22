--# -path=.:../abstract:../../prelude:../common

--1 English Lexical Paradigms
--
-- Aarne Ranta 2003--2005
--
-- This is an API for the user of the resource grammar
-- for adding lexical items. It gives functions for forming
-- expressions of open categories: nouns, adjectives, verbs.
--
-- Closed categories (determiners, pronouns, conjunctions) are
-- accessed through the resource syntax API, $Structural.gf$.
--
-- The main difference with $MorphoZul.gf$ is that the types
-- referred to are compiled resource grammar types. We have moreover
-- had the design principle of always having existing forms, rather
-- than stems, as string arguments of the paradigms.
--
-- The structure of functions for each word class $C$ is the following:
-- first we give a handful of patterns that aim to cover all
-- regular cases. Then we give a worst-case function $mkC$, which serves as an
-- escape to construct the most irregular words of type $C$.
-- However, this function should only seldom be needed: we have a
-- separate module [``IrregZul`` ../../english/IrregZul.gf],
-- which covers irregular verbss.

resource ParadigmsZul = open
  (Predef=Predef),
  Prelude,
  -- MorphoZul,
  ResZul,
  CatZul,
  ExtraCatZul,
  ParamX
  in {
--2 Parameters
--
-- -- To abstract over gender names, we define the following identifiers.
--
-- oper
--   Gender : Type ;
--
--   human     : Gender ;
--   nonhuman  : Gender ;
--   masculine : Gender ; --%
--   feminine : Gender ; --%
--
-- -- To abstract over number names, we define the following.
--
--   Number : Type ;
--
--   singular : Number ;
--   plural   : Number ;
--
-- -- To abstract over case names, we define the following.
--
--   Case : Type ; --%
--
--   nominative : Case ; --%
--   genitive   : Case ; --%
--
-- -- Prepositions are used in many-argument functions for rection.
-- -- The resource category $Prep$ is used.
--
-- -- The number of a noun phrase can be extracted with the following
-- -- function.
--
--   npNumber : NP -> Number ; -- exctract the number of a noun phrase
--
--
-- --2 Nouns
oper
  mkN = overload {
    mkN : (ngane : Str) -> ClassGender -> N  = \n,c -> lin N (regNoun n c) ;   -- "thing" nouns
    mkN : (nyaka,onyakeni,eminyakeni : Str) -> ClassGender -> N = \n,ls,lp,c -> lin N (semiRegNoun n ls lp c) ;
  } ;

  -- mkPN = overload {
  --   mkPN : Str -> Gender -> PN = \s,g -> lin PN { s = s ; a = Ag Sg Per3 g} ; -- proper name
  --   } ;

  mkA = overload {
    mkA : (kahle : Str) -> A = \a -> lin A (regAdj a) ; -- regular adjective
  } ;

  mkV = overload {
    mkV : (hamb : Str) -> V  = \hamb -> lin V (regVerb hamb) ;
    mkV : (fakw : Str) -> Voice -> V = \fakw,voice -> lin V (passiveVerb fakw voice) ;
    mkV : (th,perfsuff,suff : Str) -> V  = \hamb,perfsuff,suff -> lin V (semiRegVerb hamb perfsuff suff) ;
  } ;

  mkV2 = overload {
    mkV2 : (phuz : Str) -> V2  = \phuz -> lin V2 (regVerb phuz) ;
    mkV2 : (phathw : Str) -> Voice -> V2 = \phathw,voice -> lin V2 (passiveVerb phathw voice) ;
    mkV2 : (th,perfsuff,suff : Str) -> V2  = \hamb,perfsuff,suff -> lin V2 (semiRegVerb hamb perfsuff suff) ;
  } ;

  mkV3 = overload {
    mkV3 : (phuz : Str) -> V3  = \phuz -> lin V3 (regVerb phuz) ;
    -- mkV2 : (phathw : Str) -> Voice -> V2 = \phathw,voice -> lin V2 (passiveVerb phathw voice) ;
  } ;

  mkVA = overload {
    mkVA : (b : Str) -> VA  = \b -> lin VA (regVerb b) ;
  } ;

  mkVS = overload {
    mkVS : (cel : Str) -> VS  = \cel -> lin VS (regVerb cel) ;
  } ;

  mkVAux = overload {
    mkVAux : (hlale : Str) -> VAux  = \hlale -> lin VAux {
      s = hlale ;
      at = PartAux
    }
  } ;

  -- yourPl_Det = overload {
  --   yourPl_Det : Det = lin Det { s = "jou" ; n = Pl ; p = TPos } ;
  -- } ;
  --
  -- mkSgDet = overload {
  --   mkSgDet : Str -> Det = \s -> lin Det { s = s ; n = Sg ; p = TPos } ;
  -- } ;

  mkPlDet = overload {
    mkPlDet : Str -> Det = \s -> lin Det { s = s ; n = Pl } ;
  } ;

  -- -- mkVS = overload {
  -- --   mkVS : V -> VS = \weet -> lin VS { v = weet ; c = "dat" } ;
  -- -- } ;
  -- --
  -- -- mkVQ = overload {
  -- --   mkVQ : V -> VS = \wonder -> lin VS { v = wonder ; c = "of" } ;
  -- -- } ;

  mkAdv = overload {
    mkAdv : Str -> Adv = \adv -> lin Adv (regAdv adv) ;
    -- mkAdv : Str -> Aspect -> Adv = \adv,asp -> lin Adv (aspAdv adv asp) ;
  } ;

  mkIAdv = overload {
    mkIAdv : Str -> Adv = \adv -> lin IAdv (regAdv adv) ;
    -- mkAdv : Str -> Aspect -> Adv = \adv,asp -> lin Adv (aspAdv adv asp) ;
  } ;

  -- mkPredet = overload {
  --   mkPredet : Str -> Predet = \predet -> lin Predet { s = predet ; p = TPos } ;
  -- } ;

  -- NOTE: removing this, because it creates circularity
  -- all_Quant : QuantStem = all_QuantStem ;
} ;
