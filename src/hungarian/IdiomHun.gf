
--1 Idiom: Idiomatic Expressions

concrete IdiomHun of Idiom = CatHun ** open Prelude, ResHun, VerbHun, NounHun, StructuralHun in {

-- This module defines constructions that are formed in fixed ways,
-- often different even in closely related languages.

{-  lin


  -- ImpersCl : VP -> Cl ;        -- it is hot
  -- GenericCl : VP -> Cl ;        -- one sleeps
  ImpersCl,
  GenericCl = \vp -> predVP impersNP (passVP vp) ;

    CleftNP   : NP  -> RS -> Cl ; -- it is I who did it
    CleftAdv  : Adv -> S  -> Cl ; -- it is here she slept

  -- : NP -> Cl ;        -- there is a house
  ExistNP np =

  ExistIP   : IP -> QCl ;       -- which houses are there

-- 7/12/2012 generalizations of these

    ExistNPAdv : NP -> Adv -> Cl ;    -- there is a house in Paris
    ExistIPAdv : IP -> Adv -> QCl ;   -- which houses are there in Paris

  -- : VP -> VP ;
  ProgrVP vp = vp ** {
    } ;


  -- : VP -> Utt ;       -- let's go
  ImpPl1 vp = { } ;

  ImpP3     : NP -> VP -> Utt ; -- let John walk

-- 3/12/2013 non-reflexive uses of "self"

    SelfAdvVP : VP -> VP ;        -- is at home himself
    SelfAdVVP : VP -> VP ;        -- is himself at home
    SelfNP    : NP -> NP ;        -- the president himself (is at home)
-}

lin
  ExistNP np = existNP np [] ;

  ExistNPAdv np adv = existNP np adv.s ;

  ExistIP ip = {
    s = \\t,a,p =>
      ip.s ! NoPoss ! Nom
      ++ if_then_Pol p [] "nem"
      ++ copula.s ! case t of {
           Past => VPast P3 ip.agr.p2 ;
           _    => VPres P3 ip.agr.p2
         }
    } ;

  ExistIPAdv ip adv = {
    s = \\t,a,p =>
      ip.s ! NoPoss ! Nom
      ++ adv.s
      ++ if_then_Pol p [] "nem"
      ++ copula.s ! case t of {
           Past => VPast P3 ip.agr.p2 ;
           _    => VPres P3 ip.agr.p2
         }
    } ;

  ProgrVP vp = vp ;

  ImpPl1 vp = {
    s = "próbáljunk" ++ infVP vp
    } ;

oper
  existNP : NounPhrase -> Str -> ResHun.ClSlash = \np,adv -> {
    s = \\t,_,p =>
      if_then_Pol p [] "nem"
      ++ copula.s ! case t of {
           Past => VPast P3 np.agr.p2 ;
           _    => VPres P3 np.agr.p2
         }
      ++ linNP np
      ++ adv ;
    c2 = Acc
    } ;

}
