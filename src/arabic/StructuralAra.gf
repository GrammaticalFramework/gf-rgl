concrete StructuralAra of Structural = CatAra **
  open MorphoAra, ResAra, ParadigmsAra, (N=NounAra), Prelude in {

  flags optimize=all ;  coding=utf8 ;

  lin
  above_Prep = mkPrep "فَوْقَ" ;
  after_Prep = mkPrep "بَعْدَ" ;
  all_Predet = mkPredet "كُلّ" True ;
  almost_AdA = ss "تَقْرِيباً";
  almost_AdN = ss "حَوَالي" ; -- or  "تَقرِيبا"
--  although_Subj = ss "َلتهُْغه" ;
--  always_AdV = ss "َلوَيس" ;
  and_Conj = mkConj "وَ" Pl ;
--  because_Subj = ss "بعَُسي" ;
  before_Prep = mkPrep "قَبْلَ" ;
  behind_Prep = mkPrep "خَلْفَ" ;
  between_Prep = mkPrep "بَيْنَ" ;
  both7and_DConj = mkConj "كِلا" "وَ" Pl ;
--  but_PConj = ss "بُت" ;
  by8agent_Prep,
  by8means_Prep = biPrep ;
  can_VV = mkVV (mkV "طوع" FormX) ;
--  can8know_VV = {
--    s = table VVForm [["بي َبلي تْ"] ; "عَن" ; "عُْلد" ;
--         ["بّن َبلي تْ"] ; ["بِنغ َبلي تْ"] ; "عَنءت" ; "عُْلدنءت"] ;
--    isAux = True
--    } ;
  during_Prep = mkPrep "خِلَالَ" ;
--  either7or_DConj = sd2 "ِتهر" "ْر" ** {n = Sg} ;
  everybody_NP = regNP "الجَمِيع" Pl Def ;
  every_Det = mkDet "كُلّ" Sg Const ;
  everything_NP = regNP "كُلّ" Sg Def ;
--  everywhere_Adv = ss "ثريوهري" ;
  few_Det = mkDet "بَعض" Pl Const ;
--  first_Ord = ss "فِرست" ;
  for_Prep = liPrep ;
  from_Prep = mkPrep "مِنَ" ;
  he_Pron = ResAra.he_Pron ;
  here_Adv = ss "هُنا" ;
  here7to_Adv = ss "إلَىَ هُنَا" ;
  here7from_Adv = ss "مِنْ هُنَا" ;
  how_IAdv = ss "كَيفَ" ;
  how8many_IDet = {
    s = \\g,s,c => "كَمْ عَدَد" + caseTbl ! c ;
    n = Pl ; d = Def
    } ; -- IL
  how8much_IAdv = ss "كَمْ" ;
  if_Subj = mkSubj "إِذَا" Verbal ;
  --whether_Subj = mkSubj "مَا إِذَا" Verbal ;
  in8front_Prep = mkPrep "مُقَابِلَ" ;
  i_Pron  = ResAra.i_Pron ;
  in_Prep = mkPrep "فِي" ;
  it_Pron = N.DetNP (N.DetQuant this_Quant N.NumSg) ; -- was: it_Pron = mkPron "ِت" "ِت" "ِتس" (Per3 Masc Sg);
--  less_CAdv = ss "لسّ" ;
  many_Det = mkDet "جَمِيع" Pl Const ;
--  more_CAdv = ss "مْري" ;
  most_Predet = mkPredet  "أَكثَر" True ;
  much_Det = mkDet "الكَثِير مِنَ" Pl Const ;
  must_VV = mkVV (v1 "وجب" a i) ;
--    s = table VVForm [["بي هَثي تْ"] ; "مُست" ; ["هَد تْ"] ;
--         ["هَد تْ"] ; ["هَثِنغ تْ"] ; "مُستنءت" ; ["هَدنءت تْ"]] ; ----
--    isAux = True
--    } ;
  nothing_NP = regNP "لَا شَيْء" Sg Def ;
  no_Utt = {s = \\_ => "لا"} ;
  on_Prep = mkPrep "عَلَى" ;
  only_Predet = mkPredet "فَقَط" False;
  or_Conj = mkConj "أَوْ" ;
--  otherwise_PConj = ss "ْتهروِسي" ;
  part_Prep = mkPrep "مِنَ" ;
--  please_Voc = ss "ةلَسي" ;
  possess_Prep = liPrep ;
--  quite_Adv = ss "قُِتي" ;
  she_Pron = ResAra.she_Pron ;
  so_AdA = very_AdA ;
  somebody_NP = regNP "أَحَد" Sg Indef ;
  someSg_Det = ResAra.someSg_Det ;
  somePl_Det = ResAra.somePl_Det ;
  something_NP = regNP "شَيْء" Sg Indef ;
--  somewhere_Adv = ss "سْموهري" ;
  that_Quant = mkQuant3 "ذَلِكَ" "تِلكَ" "أُلٱِكَ" Def;
  that_Subj = mkSubj "أَنَّ" ;
----b  that_NP = indeclNP "ذَلِكَ" Sg ;
  there_Adv = ss "هُناك" ;
--  there7to_Adv = ss "تهري" ;
--  there7from_Adv = ss ["فرْم تهري"] ;
--  therefore_PConj = ss "تهرفْري" ;
----b  these_NP = indeclNP "هَؤُلَاء" Pl ;
  they_Pron = theyMasc_Pron ;
  this_Quant = mkQuant7 "هَذَا" "هَذِهِ" "هَذَان" "هَذَيْن" "هَاتَان" "هَاتَيْن" "هَؤُلَاء" Def ;
----b  this_NP = indeclNP "هَذا" Sg ;
----b  those_NP = indeclNP "هَؤُلَاءكَ" Pl ;
  through_Prep = mkPrep "عَبْرَ" ;
  too_AdA = very_AdA ;
  to_Prep = mkPrep "إِلَى" ;
  under_Prep = mkPrep "تَحْتَ" ;
  very_AdA = ss "جِدَّاً" ;
  want_VV = mkVV (mkV "رود" FormIV) ;
  we_Pron = ResAra.we_Pron ;
  whatPl_IP = mkIP "ما" "ماذا" Pl ;
  whatSg_IP = mkIP "ما" "ماذا" Sg ;
  when_IAdv = ss "مَتَى" ;
  when_Subj = mkSubj "عِنْدَمَا" Verbal ;
  where_IAdv = ss "أَينَ" ;
  which_IQuant = {
    s = \\s,c => case <c,s> of {
             <Bare,_>    => "أيّ" ;
             <Nom,Indef> => "أيٌّ" ;
             <Nom,_>     => "أيُّ" ;
             <Acc,Indef> => "أيّاً" ;
             <Acc,_>     => "أيَّ" ;
             <_Gen,Indef> => "أيٍّ" ;
             <_Gen,_>     => "أيِّ"
             }
    } ;
  whoSg_IP = mkIP "مَنْ" "مَنْ" Sg ;
  whoPl_IP = mkIP "مَنْ" "مَنْ" Pl ;
  why_IAdv = ss "لِمَاذَا" ;
  without_Prep = mkPrep "بِدُونِ" ;
  with_Prep = mkPrep "مَعَ" ;
  yes_Utt = {s = \\_ => "نَعَم"} ;
  youSg_Pron = youSgMasc_Pron ;
  youPl_Pron = youPlMasc_Pron ;
  youPol_Pron = youPlFem_Pron ; -- arbitrary?

  have_V2 = mkV2 ladaa_V (casePrep nom) ; -- "X has Y" literally "Y<nom> is on X<nom>"
  --have_V2 = dirV2 (regV "يَملِك") ; -- "X<nom> owns/possesses Y<acc>"

  lin language_title_Utt = {s = \\_ => "العربية"} ;

}
