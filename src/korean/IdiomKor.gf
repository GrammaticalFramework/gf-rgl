
--1 Idiom: Idiomatic Expressions

concrete IdiomKor of Idiom = CatKor ** open Prelude, ResKor, VerbKor, NounKor, StructuralKor in {

-- This module defines constructions that are formed in fixed ways,
-- often different even in closely related languages.

  lin


  ImpersCl vp = predVP' [] vp ;
  GenericCl vp = predVP' "사람이" vp ;

  -- CleftNP   : NP  -> RS -> Cl ; -- it is I who did it
  -- CleftAdv  : Adv -> S  -> Cl ; -- it is here she slept

  ExistNP np = predVP np (useV have_V) ;

  -- ExistIP   : IP -> QCl ;       -- which houses are there

-- 7/12/2012 generalizations of these

  ExistNPAdv np adv = predVP np (insertAdv (useV have_V) adv) ;
  -- ExistIPAdv : IP -> Adv -> QCl ;   -- which houses are there in Paris

  -- : VP -> VP ;
  ProgrVP vp = vp ** {
    s = \\vf => glue (vp.s ! VStem Pos) "고" ++ have_V.s ! vf
    } ;


  -- : VP -> Utt ;       -- let's go
  ImpPl1 vp = {s = vp.nObj ! ImpObj ++ vp.adv ++ vp.s ! VF Polite Pos} ;

  -- ImpP3     : NP -> VP -> Utt ; -- let John walk

-- 3/12/2013 non-reflexive uses of "self"

  -- SelfAdvVP : VP -> VP ;        -- is at home himself
  -- SelfAdVVP : VP -> VP ;        -- is himself at home
  -- SelfNP    : NP -> NP ;        -- the president himself (is at home)

}
