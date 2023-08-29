concrete StructuralTMP of Structural = CatTMP **
  open Prelude, ResTMP, (Noun=NounTMP), ParadigmsTMP in {

-------
-- Ad*
{-
lin almost_AdA =
lin almost_AdN =
lin at_least_AdN =
lin at_most_AdN =
lin so_AdA =
lin too_AdA =
lin very_AdA =

lin as_CAdv =
lin less_CAdv =
lin more_CAdv =

lin how8much_IAdv =
lin when_IAdv =

lin how_IAdv =
lin where_IAdv =
lin why_IAdv =

lin always_AdV = ss "" ;

lin everywhere_Adv = ss "" ;
lin here7from_Adv = ss "" ;
lin here7to_Adv = ss "" ;
lin here_Adv = ss "" ;
lin quite_Adv = ss "" ;
lin somewhere_Adv = ss "" ;
lin there7from_Adv = ss "" ;
lin there7to_Adv = ss "" ;
lin there_Adv = ss "" ;

-}
-------
-- Conj

-- The lincat of Conj is Coordination.ConjunctionDistr ** {n:Number}
-- which means that there are two fields for the strings, and
-- n:Number which specifies the number of the resulting NP.

lin and_Conj = {s1 = [] ; s2 = "and" ; n = Pl} ;
-- lin or_Conj =
-- lin if_then_Conj =
lin both7and_DConj = {s1 = "both" ; s2 = "and" ; n = Pl} ;
-- lin either7or_DConj =

-- lin but_PConj =
-- lin otherwise_PConj =
-- lin therefore_PConj =


-----------------
-- *Det and Quant
{-
lin how8many_IDet =
lin every_Det =

lin all_Predet = {s = ""} ;
lin not_Predet = { s = "" } ;
lin only_Predet = { s = "" } ;
lin most_Predet = {s = ""} ;

lin few_Det = R.indefDet "" pl ;
lin many_Det = R.indefDet "" pl ;
lin much_Det = R.indefDet "" sg ;

lin somePl_Det =
lin someSg_Det =

lin no_Quant =
lin that_Quant = mkQuant "" ;
lin this_Quant = mkQuant "" ;
lin which_IQuant = mkQuant "" ;

-----
-- NP

lin somebody_NP =


lin everybody_NP =
lin everything_NP =
lin nobody_NP =
lin nothing_NP =
lin somebody_NP =
lin something_NP =

-------
-- Prep

lin above_Prep = mkPrep "" ;
lin after_Prep = mkPrep "" ;
lin before_Prep = mkPrep "" ;
lin behind_Prep = mkPrep ""  ;
lin between_Prep = = mkPrep "" ;
lin by8agent_Prep = mkPrep "" ;
lin by8means_Prep = mkPrep "" ;
lin during_Prep = mkPrep "" ;
lin except_Prep = mkPrep "" ;
lin for_Prep = mkPrep "" ;
lin from_Prep = mkPrep "" ;
lin in8front_Prep = mkPrep "" ;
lin in_Prep = mkPrep "" ;
lin on_Prep = mkPrep "" ;
lin part_Prep = mkPrep ;
lin possess_Prep = mkPrep "" ;
lin through_Prep = mkPrep "" ;
lin to_Prep = mkPrep "k" ;
lin under_Prep = mkPrep "" ;
lin with_Prep = mkPrep "" ;
lin without_Prep = mkPrep "" ;

-------
-- Pron

-- Pronouns are closed class, no constructor in ParadigmsTMP.
lin it_Pron =
lin i_Pron =
lin youPol_Pron =
lin youSg_Pron =
lin he_Pron =
lin she_Pron =
lin we_Pron =
lin youPl_Pron =
lin they_Pron =

lin whatPl_IP =
lin whatSg_IP =
lin whoPl_IP =
lin whoSg_IP =

-------
-- Subj

lin although_Subj =
lin because_Subj =
lin if_Subj =
lin that_Subj =
lin when_Subj =


------
-- Utt

lin language_title_Utt = ss "" ;
lin no_Utt = ss "" ;
lin yes_Utt = ss "" ;


-------
-- Verb

lin have_V2 =

lin can8know_VV =  -- can (capacity)
lin can_VV =  -- can (possibility)
lin must_VV =
lin want_VV =

------
-- Voc

lin please_Voc = ss "" ;
-}

}
