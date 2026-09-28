concrete StructuralGla of Structural = CatGla **
  open Prelude, ResGla, (Noun=NounGla), ParadigmsGla in {

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

lin and_Conj = {s1 = [] ; s2 = "agus" ; n = Pl} ;
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

-------}
-- Prep

-- lin above_Prep = mkPrep "" ;
-- lin after_Prep = mkPrep "" ;
-- lin before_Prep = mkPrep "" ;
-- lin behind_Prep = mkPrep ""  ;
-- lin between_Prep = = mkPrep "" ;
-- lin by8agent_Prep = mkPrep "" ;
-- lin by8means_Prep = mkPrep "" ;
-- lin during_Prep = mkPrep "" ;
-- lin except_Prep = mkPrep "" ;
lin for_Prep = ResGla.doPrep ;
lin from_Prep = ResGla.bhoPrep ;
-- lin in8front_Prep = mkPrep "" ;
lin in_Prep = ResGla.annPrep ;
lin on_Prep = ResGla.airPrep ;
-- lin part_Prep = mkPrep "" ;
-- lin possess_Prep = mkPrep "" ;
-- lin through_Prep = mkPrep "" ;
lin to_Prep = ResGla.guPrep ;
-- lin under_Prep = mkPrep "" ;
-- lin with_Prep = mkPrep "" ;
-- lin without_Prep = mkPrep "" ;

-------
-- Pron

-- Pronouns are closed class, no constructor in ParadigmsGla.
--lin it_Pron =
lin i_Pron = mkPron "mi" "mo" Sg1 ;
lin youPol_Pron = youPl_Pron ;
lin youSg_Pron = mkPron "thu" "do" Sg2 ;
lin he_Pron = mkPron "e" "a" (Sg3 Masc) ;
lin she_Pron = mkPron "i" "a" (Sg3 Fem) ;
lin we_Pron = mkPron "sinn" "àr" Pl1 ;
lin youPl_Pron = mkPron"sibh" "ur" Pl2 ;
lin they_Pron = mkPron "iad" AN Pl3 ;
{-
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

lin
  almost_AdA = {s = "cha mhòr"} ;
  almost_AdN = {s = "cha mhòr"} ;
  at_least_AdN = {s = "co-dhiù"} ;
  at_most_AdN = {s = "air a' char as motha"} ;
  so_AdA = {s = "cho"} ;
  too_AdA = {s = "ro"} ;
  very_AdA = {s = "glè"} ;
  as_CAdv = {s = "cho" ; p = "ri"} ;
  less_CAdv = {s = "nas lugha" ; p = "na"} ;
  more_CAdv = {s = "nas" ; p = "na"} ;

  how8much_IAdv = {s = "dè an uiread"} ;
  how_IAdv = {s = "ciamar"} ;
  when_IAdv = {s = "cuin"} ;
  where_IAdv = {s = "càite"} ;
  why_IAdv = {s = "carson"} ;
  always_AdV = {s = "an-còmhnaidh"} ;
  everywhere_Adv = {s = "anns gach àite"} ;
  here7from_Adv = {s = "à seo"} ;
  here7to_Adv = {s = "an seo"} ;
  here_Adv = {s = "an seo"} ;
  quite_Adv = {s = "gu math"} ;
  somewhere_Adv = {s = "an àiteigin"} ;
  there7from_Adv = {s = "à sin"} ;
  there7to_Adv = {s = "an sin"} ;
  there_Adv = {s = "an sin"} ;

  or_Conj = {s1 = [] ; s2 = "no" ; n = Sg} ;
  if_then_Conj = {s1 = "ma" ; s2 = "an uair sin" ; n = Sg} ;
  either7or_DConj = {s1 = "an dara cuid" ; s2 = "no" ; n = Sg} ;
  but_PConj = {s = "ach"} ;
  otherwise_PConj = {s = "air neo"} ;
  therefore_PConj = {s = "mar sin"} ;

  every_Det = ParadigmsGla.mkDet "gach" Sg Indef ;
  few_Det = ParadigmsGla.mkDet "beagan" Pl Indef ;
  many_Det = ParadigmsGla.mkDet "mòran" Pl Indef ;
  much_Det = ParadigmsGla.mkDet "mòran" Sg Indef ;
  somePl_Det = ParadigmsGla.mkDet "cuid de" Pl Indef ;
  someSg_Det = ParadigmsGla.mkDet "rudeigin de" Sg Indef ;
  how8many_IDet = {s = "cia mheud"} ;
  all_Predet = {s = "uile"} ;
  most_Predet = {s = "a' mhòr-chuid de"} ;
  not_Predet = {s = "chan e"} ;
  only_Predet = {s = "a-mhàin"} ;
  no_Quant = ParadigmsGla.mkQuant "gun" Indef ;
  that_Quant = ParadigmsGla.mkQuant "sin" Def ;
  this_Quant = ParadigmsGla.mkQuant "seo" Def ;
  which_IQuant = {s = "dè"} ;

  everybody_NP = atomNP "a h-uile duine" Pl3 ;
  everything_NP = atomNP "a h-uile rud" (Sg3 Masc) ;
  nobody_NP = atomNP "duine sam bith" (Sg3 Masc) ;
  nothing_NP = atomNP "rud sam bith" (Sg3 Masc) ;
  somebody_NP = atomNP "cuideigin" (Sg3 Masc) ;
  something_NP = atomNP "rudeigin" (Sg3 Masc) ;

  above_Prep = simplePrep "os cionn" Gen ;
  after_Prep = simplePrep "an dèidh" Gen ;
  before_Prep = simplePrep "ro" (Dat Lenited) ;
  behind_Prep = simplePrep "air cùl" Gen ;
  between_Prep = simplePrep "eadar" (Dat NoMutation) ;
  by8agent_Prep = simplePrep "le" (Dat NoMutation) ;
  by8means_Prep = simplePrep "le" (Dat NoMutation) ;
  during_Prep = simplePrep "rè" Gen ;
  except_Prep = simplePrep "ach" (Nom NoMutation) ;
  in8front_Prep = simplePrep "air beulaibh" Gen ;
  part_Prep = simplePrep "de" (Dat Lenited) ;
  possess_Prep = simplePrep "aig" (Dat NoMutation) ;
  through_Prep = simplePrep "tro" (Dat NoMutation) ;
  under_Prep = simplePrep "fo" (Dat Lenited) ;
  with_Prep = simplePrep "le" (Dat NoMutation) ;
  without_Prep = simplePrep "gun" (Dat Lenited) ;

  it_Pron = mkPron "e" "a" (Sg3 Masc) ;
  whatPl_IP = {s = "dè"} ;
  whatSg_IP = {s = "dè"} ;
  whoPl_IP = {s = "cò"} ;
  whoSg_IP = {s = "cò"} ;

  although_Subj = {s = "ged"} ;
  because_Subj = {s = "oir"} ;
  if_Subj = {s = "ma"} ;
  that_Subj = {s = "gun"} ;
  when_Subj = {s = "nuair a"} ;

  language_title_Utt = {s = "Gàidhlig"} ;
  no_Utt = {s = "chan eil"} ;
  yes_Utt = {s = "tha"} ;
  please_Voc = {s = "mas e do thoil e"} ;

  have_V2 = biV ** {c2 = aigPrep} ;
  can8know_VV = mkV "urrainn" ;
  can_VV = mkV "faod" ;
  must_VV = mkV "feum" ;
  want_VV = mkV "iarr" ;

oper
  biV : LinV = {
    s = "bi" ;
    conditional = table {Sg => "bhiodh" ; Pl => "bhiodh"} ;
    imperative = table {
      P1 => table {Sg => "bitheam" ; Pl => "bitheamaid"} ;
      P2 => table {Sg => "bi" ; Pl => "bithibh"} ;
      P3 => table {Sg => "bitheadh" ; Pl => "bitheadh"}
      } ;
    future = table {Indep => "bidh" ; Dep => "bi"} ;
    past = table {Indep => "bha" ; Dep => "robh"} ;
    noun = "bhith" ; participle = "air a bhith" ;
    copular = True ; complement = []
    } ;

  atomNP : Str -> PronAgr -> LinNP = \s,a -> emptyNP ** {
    s = \\_ => s ; voc = s ; a = IsPron a
    } ;

}
