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
lin few_Det = mkDet "kevés" Def Sg ; -- TODO check
lin many_Det = mkDet "sok" Def Sg ; -- TODO check
--lin much_Det =

lin somePl_Det = mkDet "néhány" Indef Sg ;
lin someSg_Det = mkDet "néhány" Indef Sg ;
--lin no_Quant =

lin that_Quant = mkQuant "az" "az" ;
lin this_Quant = mkQuant "ez" "ez" ;
{-lin which_IQuant =


-----
-- NP

lin everybody_NP = defNP "" N.NumPl ;
lin everything_NP = defNP "" N.NumSg ;
lin nobody_NP = mkVerb; ""
lin nothing_NP = defNP "" N.NumSg ;
lin somebody_NP = defNP "" N.NumSg ;
-}
lin something_NP = defNP "valami" Sg ;

-------
-- Prep

-- List of postpositions requiring case:
-- https://en.wiktionary.org/wiki/Appendix:Hungarian_postpositions#Postpositions_Requiring_Case
lin above_Prep = mkPrep "fölött" ;
-- lin after_Prep = mkPrep ""
-- lin before_Prep = mkPrep "" ;
-- lin behind_Prep = mkPrep "" ;
-- lin between_Prep = = mkPrep "" ;
lin by8agent_Prep = mkPrep "által" ;
lin by8means_Prep = casePrep Ins ;
-- lin during_Prep = mkPrep ;
-- lin except_Prep = mkPrep ;
-- lin for_Prep = mkPrep "" ;
-- lin from_Prep = mkPrep "" ;
-- lin in8front_Prep = mkPrep "" ;
lin in_Prep = casePrep Ine ;
lin on_Prep = casePrep Ade ;
-- lin part_Prep = casePrep  ;
-- lin possess_Prep = -- Suffix attaches to possessee, not possessor
-- lin through_Prep = mkPrep ;
lin to_Prep = casePrep All ;
lin under_Prep = mkPrep "alatt" ;
-- lin with_Prep = mkPrep "" ;
-- lin without_Prep = mkPrep "" ;


-------
-- Pron

-- Pronouns are closed class, no constructor in ParadigmsHun.
  -- it_Pron =
  i_Pron = emptyNP ** {
    s = caseTable "én" "engem" "nekem"
                  "belém" "bennem" "belőlem" -- inner locatives
                  "hozzám" "nálam" "tőlem"   -- outer locatives
                  "rám" "rajtam" "rólam"     -- outer locatives
                  "értem" -- Causative
                  "velem" -- Instrumental
                  nonExist ; -- Translative
    agr = <P1,Sg> ;
    objdef = Def ;
    poss = "em" ;
    } ;
  youPol_Pron,
  youSg_Pron = emptyNP ** {
    s = caseTable "te" "teged" "neked"
                  "beléd" "benned" "belőled"
                  "hozzád" "nálad" "tőled"
                  "rád" "rajtad" "rólad"
                  "érted" -- Causative
                  "veled" -- Instrumental
                  nonExist ; -- Translative
    agr = <P2,Sg> ;
    objdef = Def ;
    poss = "d" ;
    } ;
  he_Pron,
  she_Pron = emptyNP ** {
    s = caseTable "ő" "őt" "neki"
                  "belé" "benne" "belőle"
                  "hozzá" "nála" "tőle"
                  "rá" "rajta" "róla"
                  "érte" -- Causative
                  "vele" -- Instrumental
                  nonExist ; -- Translative
    objdef = Def ;
    } ;
  we_Pron = emptyNP ** {
    s = caseTable "mi" "minket" "nekünk"
                  "belénk" "bennünk" "belőlünk"
                  "hozzánk" "nálunk" "tőlünk"
                  "ránk" "rajtunk" "rólunk"
                  "értünk" -- Causative
                  "velünk" -- Instrumental
                  nonExist ; -- Translative
    agr = <P1,Pl> ;
    objdef = Def ;
    } ;

  youPl_Pron = emptyNP ** {
    s = caseTable "ti" "titeket" "nektek"
                  "belétek" "bennetek" "belőletek"
                  "hozzátok" "nálatok" "tőletek"
                  "rátok" "rajtatok" "rólatok"
                  "értetek" -- Causative
                  "veletek" -- Instrumental
                  nonExist ; -- Translative
    agr = <P2,Pl> ;
    objdef = Def ;
    } ;
  they_Pron = emptyNP ** {
    s = caseTable "ők" "őket" "nekik"
                  "beléjük" "bennük" "belőlük"
                  "hozzájuk" "náluk" "tőlük"
                  "rájuk" "rajtuk" "róluk"
                  "értük" -- Causative
                  "velük" -- Instrumental
                  nonExist ; -- Translative
    agr = <P3,Pl> ;
    objdef = Def ;
    } ;

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
