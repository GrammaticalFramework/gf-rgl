
--1 Idiom: Idiomatic Expressions

concrete IdiomSom of Idiom = CatSom ** open Prelude, ResSom, VerbSom, NounSom, StructuralSom in {

-- This module defines constructions that are formed in fixed ways,
-- often different even in closely related languages.

  lin

  -- ImpersCl : VP -> Cl ;        -- it is hot
  -- GenericCl : VP -> Cl ;        -- one sleeps
  ImpersCl,
  GenericCl = \vp -> predVP impersNP (passVP vp) ;

{-
    CleftNP   : NP  -> RS -> Cl ; -- it is I who did it
    CleftAdv  : Adv -> S  -> Cl ; -- it is here she slept
    -}
  -- : NP -> Cl ;        -- there is a house
  ExistNP np =
     let vp = UseComp (CompNP np)
      in predVP impersNP vp ;

{-    ExistIP   : IP -> QCl ;       -- which houses are there

-- 7/12/2012 generalizations of these

    ExistNPAdv : NP -> Adv -> Cl ;    -- there is a house in Paris
    ExistIPAdv : IP -> Adv -> QCl ;   -- which houses are there in Paris
-}
  -- : VP -> VP ;
  ProgrVP vp = vp ** {
    s = table {
          VPres _ agr pol => vp.s ! VPres Progressive agr pol ;
          VPast _ agr => vp.s ! VPast Progressive agr ;
          VNegPast _ => vp.s ! VNegPast Progressive ;
          x => vp.s ! x }
    } ;


  {- TODO: Saeed p. 92 and 207, optative
  -- : VP -> Utt ;       -- let's go
  ImpPl1 vp = { } ;

  ImpP3     : NP -> VP -> Utt ; -- let John walk

-- 3/12/2013 non-reflexive uses of "self"

    SelfAdvVP : VP -> VP ;        -- is at home himself
    SelfAdVVP : VP -> VP ;        -- is himself at home
    SelfNP    : NP -> NP ;        -- the president himself (is at home)
-}

}
