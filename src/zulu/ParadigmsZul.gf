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
  CatExtZul,
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
    mkN : (khaya, khaya : Str) -> ClassGender -> N  = \n,n2,c -> lin N (mkELocN n c) ;   -- "thing" nouns
    mkN : (nyaka,onyakeni,eminyakeni : Str) -> ClassGender -> N = \n,ls,lp,c -> lin N (semiRegNoun n ls lp c) ;
    mkN : (iso,amehlo,esweni,emehlweni : Str) -> ClassGender -> N = \ns,np,ls,lp,c -> lin N (mkNoun ns np ls lp c) ;
  } ;

  mkPN = overload {
    mkPN : (John : Str) -> N  = \n -> lin N (regNoun n C1a_2a) ;
    mkPN : (John : Str) -> ClassGender -> N  = \n,c -> lin N (regNoun n c) ;   -- "thing" nouns
    mkPN : (nyaka,onyakeni,eminyakeni : Str) -> ClassGender -> N = \n,ls,lp,c -> lin N (semiRegNoun n ls lp c) ;
    mkPN : (iso,amehlo,esweni,emehlweni : Str) -> ClassGender -> N = \ns,np,ls,lp,c -> lin N (mkNoun ns np ls lp c) ;
    } ;

  mkKwaPN = overload {
    mkKwaPN : (Zulu : Str) -> ClassGender -> PN = \n,c -> lin PN (kwaProperName n c) ;
  } ;

  mkAdA = overload {
    mkAdA : (kakhulu : Str ) -> AdA = \a -> lin AdA (ss a) ;
  } ;

  mkA = overload {
    mkA : (kahle : Str) -> A = \a -> lin A (regAdj a) ; -- regular adjective
  } ;

  mkRelA = overload {
    mkRelA : (mnandi : Str) -> A = \a -> lin A (relAdj a) ; -- relative stem "adjective"
  } ;

  -- mkEnumA = overload {
  --   mkEnumA : (mbe : Str) -> A = \a -> lin A (enumAdj a) ; -- relative stem "adjective"
  -- } ;

  mkV = overload {
    mkV : (hamb : Str) -> V  = \hamb -> lin V (regVerb hamb) ;
    mkV : (th,thi : Str) -> V  = \th,thi -> lin V (th_Verb th thi) ;
    mkV : (guqubal,guqubala,guqubele : Str) -> V = \guqubal,guqubala,guqubele -> lin V (three_Verb guqubal guqubala guqubele) ;
    mkV : (dlal,dlala,dlalile,dlale : Str) -> V = \dlal,dlala,dlalile,dlale -> lin V (four_Verb dlal dlala dlalile dlale) ;
  } ;

  passV = overload {
    passV: Str -> V = \fakw -> lin V (passiveVerb fakw) ;
  } ;

  passV2 = overload {
    passV2: Str -> V2 = \fakw -> lin V2 (passiveVerb fakw) ;
  } ;

  mkV2 = overload {
    mkV2 : (hamb : Str) -> V2  = \hamb -> lin V2 (regVerb hamb) ;
    mkV2 : (th,thi : Str) -> V2  = \th,thi -> lin V2 (th_Verb th thi) ;
    mkV2 : (guqubal,guqubala,guqubele : Str) -> V2 = \guqubal,guqubala,guqubele -> lin V2 (three_Verb guqubal guqubala guqubele) ;
    mkV2 : (guqubal,guqubala,guqubele,guqubele_2 : Str) -> V2 = \guqubal,guqubala,guqubele,guqubele_2 -> lin V2 (four_Verb guqubal guqubala guqubele guqubele_2) ;
  } ;

  mkV3 = overload {
    mkV3 : (hamb : Str) -> V3  = \hamb -> lin V3 (regVerb hamb) ;
    mkV3 : (th,thi : Str) -> V3  = \th,thi -> lin V3 (th_Verb th thi) ;
    mkV3 : (guqubal,guqubala,guqubele : Str) -> V3 = \guqubal,guqubala,guqubele -> lin V3 (three_Verb guqubal guqubala guqubele) ;
    mkV3 : (guqubal,guqubala,guqubele,guqubele_2 : Str) -> V3 = \guqubal,guqubala,guqubele,guqubele_2 -> lin V3 (four_Verb guqubal guqubala guqubele guqubele_2) ;
  } ;

--  mkV2V = overload {
--    mkV2V : (hamb : Str) -> V2V  = \hamb -> lin V2V (regVerb hamb) ;
--    mkV2V : (th,thi : Str) -> V2V  = \th,thi -> lin V2V (th_Verb th thi) ;
--    mkV2V : (guqubal,guqubala,guqubele : Str) -> V2V = \guqubal,guqubala,guqubele -> lin V2V (three_Verb guqubal guqubala guqubele) ;
--    mkV2V : (guqubal,guqubala,guqubele,guqubele_2 : Str) -> V2V = \guqubal,guqubala,guqubele,guqubele_2 -> lin V2V (four_Verb guqubal guqubala guqubele guqubele_2) ;
--  } ;

  mkVA = overload {
    mkVA : (b : Str) -> VA  = \b -> lin VA (regVerb b) ;
  } ;

  mkVV = overload {
    mkVV : (f : Str) -> VV  = \f -> lin VV (regVerb f)
  } ;

  -- mkVS = overload {
  --   mkVS : (cel : Str) -> SType -> VS  = \cel,st -> lin VS ((regVerb cel) ** { s_type = st } ) ;
  --   mkVS : (az,azi : Str) -> SType -> VS  = \az,azi,st -> lin VS ((th_Verb az azi) ** { s_type = st } ) ;
  -- } ;

  -- yourPl_Det = overload {
  --   yourPl_Det : Det = lin Det { s = "jou" ; n = Pl ; p = TPos } ;
  -- } ;
  --
  -- mkSgDet = overload {
  --   mkSgDet : Str -> Det = \s -> lin Det { s = s ; n = Sg ; p = TPos } ;
  -- } ;

  -- mkPlDet = overload {
  --   mkPlDet : Str -> Det = \s -> lin Det { s = s ; n = Pl ; qdef = Article Def } ;
  -- } ;

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
    mkIAdv : Str -> Bool -> IAdv = \adv,b -> lin IAdv { s = "ingabe" ; postIAdv = b } ;
    -- mkAdv : Str -> Aspect -> Adv = \adv,asp -> lin Adv (aspAdv adv asp) ;
  } ;

  -- mkPredet = overload {
  --   mkPredet : Str -> Predet = \predet -> lin Predet { s = predet ; p = TPos } ;
  -- } ;
} ;
