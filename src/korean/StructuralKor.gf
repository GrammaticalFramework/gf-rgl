concrete StructuralKor of Structural = CatKor **
  open Prelude, ResKor, (N=NounKor), ParadigmsKor in {

-------
-- Ad*

-- lin almost_AdA = mkAdA "" ;
-- lin almost_AdN = ss "" ;
-- lin at_least_AdN = ss "" ;
-- lin at_most_AdN = ss "" ;
-- lin so_AdA = ss "" ;
-- lin too_AdA = ss "" ;
lin very_AdA = mkAdA "매우" ;
{-
lin as_CAdv = { s = "" ; p = [] } ;
lin less_CAdv = { s = "" ; p = [] } ;
lin more_CAdv = { s = "" ; p = [] } ;
lin how_IAdv = ss "" :

lin how8much_IAdv = ss "" ;
lin when_IAdv = ss "" ;
lin where_IAdv = ss "" :
lin why_IAdv = ss "" :
-}
lin always_AdV = ss "항상" ;
{-
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

lin and_Conj = {
    s1 = [] ;
    -- no need for string, actual values come from ParamKor.conjTable
    s2 = [] ; -- this is only used in the base/cons functions with comma.
              -- another application can use commas and just one conjunction.
    n = Pl ;
    c = And
    } ;
lin or_Conj = {s1,s2 = [] ; n = Sg ; c = Or} ;
-- lin if_then_Conj = mkConj
-- lin both7and_DConj = mkConj "" "" pl ;
-- lin either7or_DConj = {s2 = \\_ => "" ; s1 = "" ; n = Sg} ;
--
-- lin but_PConj = ss "" ;
-- lin otherwise_PConj = ss "" ;
-- lin therefore_PConj = ss "" ;


-----------------
-- *Det and Quant


-- lin how8many_IDet = ;

lin all_Predet = {s = \\_ => "마다" ; p = Vowel} ;
--lin not_Predet = { s = "" } ;
lin only_Predet = {s = \\_ => "만" ; p = Consonant} ;
--lin most_Predet = {s = ""} ;


--lin every_Det = ;
lin few_Det = mkDet "조금의" Pl ;
lin many_Det = mkDet "많은" Pl ;
-- lin much_Det = ;

lin somePl_Det = mkDet "어떤" Pl ;
lin someSg_Det = mkDet "어떤" Sg ;
--lin no_Quant =

lin that_Quant = mkQuant "그" "그것" ;
lin this_Quant = mkQuant "이" "이것" ;
{-lin which_IQuant =


-----
-- NP

lin everybody_NP = defNP "" N.NumPl ;
lin everything_NP = defNP "" N.NumSg ;
lin nobody_NP = mkVerb; ""
lin nothing_NP = defNP "" N.NumSg ;
lin somebody_NP = defNP "" N.NumSg ;
-}
lin something_NP = mkNoun "무엇" ;

-------
-- Prep

-- lin above_Prep = mkPrep ""
-- lin after_Prep = mkPrep ""
-- lin before_Prep = mkPrep "" ;
lin behind_Prep = mkPrep "뒤에" False ;
-- lin between_Prep = = mkPrep "" ;
lin by8agent_Prep = mkPrep "에 의해" ; -- TODO check
lin by8means_Prep = mkPrep "에 인해" ; -- TODO check
-- lin during_Prep = mkPrep ;
-- lin except_Prep = mkPrep ;
lin for_Prep = mkPrep "에게" ;
-- lin from_Prep = mkPrep "" ;
-- lin in8front_Prep = mkPrep "" ;
lin in_Prep = mkPrep "에서" ;
-- lin on_Prep = mkPrep "에서" ;
lin part_Prep = mkPrep "의" ;
lin possess_Prep = mkPrep "의" ;
-- lin through_Prep = mkPrep ;
lin to_Prep = mkPrep "에";
-- lin under_Prep = mkPrep "" ;
lin with_Prep = mkPrep "와" "과" ;
-- lin without_Prep = mkPrep "" ;


-------
-- Pron

-- Pronouns are closed class, no constructor in ParadigmsKor.
  -- it_Pron =
  i_Pron = let iReg : Pronoun = mkPron "저" "제" in
    iReg ** {s = table {Subject => "제가"; nf => iReg.s ! nf}
    } ;
  youPol_Pron = mkPron "당신" ;
  youSg_Pron = mkPron "너" ; -- NB. this is very informal, best not used
  he_Pron = mkPron "그" ;
  she_Pron = mkPron "그녀" ;
  -- we_Pron =
  youPl_Pron = mkPron "너희" ;
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

lin language_title_Utt = ss "한국어" ;
lin no_Utt = ss "예" ;
lin yes_Utt = ss "아니요" ;


-------
-- Verb

lin have_V2 = vtov2 have_V ** {sc = Topic ; c2 = Subject} ;
-- lin can8know_VV = can_VV ; -- can (capacity)
-- lin can_VV = mkVV "" ;   -- can (possibility)
-- lin must_VV = mkVV "" ;
-- lin want_VV = mkVV "" subjunctive ;


------
-- Voc
{-
lin please_Voc = ss "" ;
-}

}
