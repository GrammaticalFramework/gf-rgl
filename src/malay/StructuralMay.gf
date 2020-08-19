concrete StructuralMay of Structural = CatMay **
  open Prelude, ResMay, (N=NounMay), ParadigmsMay in {

-------
-- Ad*
{-
lin almost_AdA = mkAdA "" ;
lin almost_AdN = ss "" ;
lin at_least_AdN = ss "" ;
lin at_most_AdN = ss "" ;
lin so_AdA = ss "" ;
lin too_AdA = ss "" ;
lin very_AdA = mkAdA "" ;

lin as_CAdv = { s = "" ; p = [] } ;
lin less_CAdv = { s = "" ; p = [] } ;
lin more_CAdv = { s = "" ; p = [] } ;
lin how_IAdv = ss "" :

lin how8much_IAdv = ss "" ;
lin when_IAdv = ss "" ;
lin where_IAdv = ss "" :
lin why_IAdv = ss "" :

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

-- lin and_Conj = {s2 = table {x => "" ; y => ""} ; s1 = [] ; n = Pl} ;
-- lin or_Conj = {s2 = \\_ => "" ; s1 = [] ; n = Sg} ;
-- lin if_then_Conj = mkConj
-- lin both7and_DConj = mkConj "" "" pl ;
-- lin either7or_DConj = {s2 = \\_ => "" ; s1 = "" ; n = Sg} ;
--
-- lin but_PConj = ss "" ;
-- lin otherwise_PConj = ss "" ;
-- lin therefore_PConj = ss "" ;


-----------------
-- *Det and Quant

{-
lin how8many_IDet = ;

lin all_Predet = {s = ""} ;
lin not_Predet = { s = "" } ;
lin only_Predet = { s = "" } ;
lin most_Predet = {s = ""} ;

lin every_Det = {s = ""} ;
lin few_Det = R.indefDet "" pl ;
lin many_Det = R.indefDet "" pl ;
lin much_Det = R.indefDet "" sg ;

lin somePl_Det =
lin someSg_Det =

lin no_Quant = -}
lin that_Quant = mkQuant "itu" ;
lin this_Quant = mkQuant "ini" ;
{-lin which_IQuant =


-----
-- NP

lin everybody_NP = defNP "" N.NumPl ;
lin everything_NP = defNP "" N.NumSg ;
lin nobody_NP = mkVerb; ""
lin nothing_NP = defNP "" N.NumSg ;
lin somebody_NP = defNP "" N.NumSg ;
lin something_NP = defNP "" N.NumSg ;

oper
 defNP : Str -> Num -> NP = {} ;
-}

-------
-- Prep

-- lin above_Prep = mkPrep ""
-- lin after_Prep = mkPrep ""
-- lin before_Prep = mkPrep "" ;
-- lin behind_Prep = mkPrep ""  ;
-- lin between_Prep = = mkPrep "" ;
lin by8agent_Prep = mkPrep "oleh" ; -- for pronoun agent, see Mintz p. 170, 5.4.1
lin by8means_Prep = mkPrep "dengan" ;
-- lin during_Prep = mkPrep ;
-- lin except_Prep = mkPrep ;
-- lin for_Prep = mkPrep ;
-- lin from_Prep = mkPrep "" ;
-- lin in8front_Prep = mkPrep "" ;
lin in_Prep = mkPrep "di" ;
-- lin on_Prep = mkPrep "" ;
-- lin part_Prep = mkPrep ;
-- lin possess_Prep = mkPrep ;
-- lin through_Prep = mkPrep ;
lin to_Prep = mkPrep "ke" ;
-- lin under_Prep = mkPrep "" ;
lin with_Prep = mkPrep "dengan" ;
-- lin without_Prep = mkPrep "" ;


-------
-- Pron

-- Pronouns are closed class, no constructor in ParadigmsMay.
  -- it_Pron =
  -- i_Pron =
  -- youPol_Pron,
  -- youSg_Pron =
  -- he_Pron =
  -- she_Pron =
  -- we_Pron =
  -- youPl_Pron =
  -- they_Pron =

--lin whatPl_IP = ;
--lin whatSg_IP = :
--lin whoPl_IP = ;
--lin whoSg_IP = ;

-------
-- Subj

-- lin although_Subj =
-- lin because_Subj =
-- lin if_Subj =
-- lin that_Subj =
-- lin when_Subj =


------
-- Utt

lin language_title_Utt = ss "bahasa Melayu" ;
-- lin no_Utt = ss "" ;
-- lin yes_Utt = ss "" ;


-------
-- Verb

--lin have_V2 = mkV2 have_V ;
-- lin can8know_VV = can_VV ; -- can (capacity)
-- lin can_VV = mkVV "" ;   -- can (possibility)
-- lin must_VV = mkVV "" ;
lin want_VV = mkVV "mahu" ;

------
-- Voc
{-
lin please_Voc = ss "" ;
-}

}
