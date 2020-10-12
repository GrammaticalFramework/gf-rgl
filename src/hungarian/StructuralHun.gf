concrete StructuralHun of Structural = CatHun **
  open Prelude, ResHun, ParadigmsHun in {
{-
-------
-- Ad*

lin almost_AdA = mkAdA "" ;
lin almost_AdN = ss "" ;
lin at_least_AdN = ss "" ;
lin at_most_AdN = ss "" ;
lin so_AdA = mkAdA "" ;
lin too_AdA = mkAdA "" ;
lin very_AdA = mkAdA "" ;
-}
lin as_CAdv = {s = "olyan" ; p = "mint"} ;
{-
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

lin and_Conj = mkConj "és" Pl ;
lin or_Conj = mkConj "vagy" Sg ;
-- lin if_then_Conj =
-- lin both7and_DConj =
lin either7or_DConj = mkDConj "vagy" "vagy" Sg ;
{-
lin but_PConj = ss "" ;
lin otherwise_PConj = ss "" ;
lin therefore_PConj = ss "" ;

-----------------
-- *Det and Quant


lin how8many_IDet = ;

lin all_Predet = {s = ""} ;
lin not_Predet = {s = ""} ;
lin only_Predet = {s = ""} ;
lin most_Predet = {s = ""} ;
-}

--lin every_Det =
lin few_Det = mkDet "kevés" Indef Sg False ;
lin many_Det = mkDet "sok" Indef Sg False ;
--lin much_Det =

lin someSg_Det,
    somePl_Det = mkDet2 "néhány" "néhányat" Indef Sg False ;
--lin no_Quant =

lin that_Quant =
  let az : Quant = mkQuant "az" "az" ;
   in az ** {s = \\n,c => az.s ! n ! c ++ pre {"a" ; "az" / v }} ;
lin this_Quant =
  let ez : Quant = mkQuant "ez" "ez" ;
   in ez ** {s = \\n,c => ez.s ! n ! c ++ pre {"a" ; "az" / v }} ;
{-lin which_IQuant =


-----
-- NP

lin everybody_NP = defNP "" N.NumPl ;
lin everything_NP = defNP "" N.NumSg ;
lin nobody_NP = mkVerb; ""
lin nothing_NP = defNP "" N.NumSg ;
lin somebody_NP = defNP "" N.NumSg ;
-}
lin something_NP = defNPPrefix "vala" "mi" Sg ; -- vowel harmony according to mi

-------
-- Prep

-- List of postpositions requiring case:
-- https://en.wiktionary.org/wiki/Appendix:Hungarian_postpositions#Postpositions_Requiring_Case
lin above_Prep = nomAdp "fölött" ;
-- lin after_Prep = mkPrep ""
-- lin before_Prep = mkPrep "" ;
-- lin behind_Prep = mkPrep "" ;
-- lin between_Prep = = mkPrep "" ;
lin by8agent_Prep = nomAdp "által" ;
lin by8means_Prep = caseAdp Ins ;
-- lin during_Prep = mkPrep ;
-- lin except_Prep = mkPrep ;
lin for_Prep = caseAdp Dat ;
-- lin from_Prep = mkPrep "" ;
-- lin in8front_Prep = mkPrep "" ;
lin in_Prep = caseAdp Ine ;
lin on_Prep = caseAdp Sup ;
-- lin part_Prep = casePrep  ;
-- lin possess_Prep = -- Suffix attaches to possessee, not possessor
-- lin through_Prep = mkPrep ;
lin to_Prep = caseAdp All ;
lin under_Prep = nomAdp "alatt" ;
-- lin with_Prep = mkPrep "" ;
-- lin without_Prep = mkPrep "" ;


-------
-- Pron

-- Pronouns are closed class, no constructor in ParadigmsHun.
  -- it_Pron =
  i_Pron = pronTable ! <P1,Sg> ;
  youPol_Pron,
  youSg_Pron = pronTable ! <P2,Sg> ;
  he_Pron,
  she_Pron = pronTable ! <P3,Sg> ;
  we_Pron = pronTable ! <P1,Pl> ;
  youPl_Pron = pronTable ! <P2,Pl> ;
  they_Pron = pronTable ! <P3,Pl> ;

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

lin language_title_Utt = ss "magyar" ;
lin no_Utt = ss "nem" ;
lin yes_Utt = ss "igen" ;

-------
-- Verb

lin have_V2 = datV2 copula ;
-- uncomment if prefer def obj with megvan
--  ** {
--  s = table {Indef => copula.s } ;
-- -           Def   => megvan.s } ;
--  } ;
{-lin can8know_VV = can_VV ; -- can (capacity)
lin can_VV = mkVV "" ;   -- can (possibility)
lin must_VV = mkVV "" ;
lin want_VV = mkVV "" subjunctive ;

------
-- Voc

lin please_Voc = ss "" ;
-}

}
