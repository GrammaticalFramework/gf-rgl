concrete StructuralAra of Structural = CatAra **
  open MorphoAra, ResAra, ParadigmsAra, Prelude in {

  flags optimize=all ;  coding=utf8 ;

  lin
  above_Prep = mkPrep "فَوْقَ" ;
  after_Prep = mkPrep "بَعْدَ" ;
  all_Predet = mkPredet "كُلّ" True ;
  almost_AdA = ss "تَقْرِيباً";
  almost_AdN = ss "حَوَالي" ; -- or  "تَقرِيبا"
--  although_Subj = ss "َلتهُْغه" ;
--  always_AdV = ss "َلوَيس" ;
  and_Conj = ss "وَ" ** {n = Pl} ;
--  because_Subj = ss "بعَُسي" ;
  before_Prep = mkPrep "قَبْلَ" ;
  behind_Prep = mkPrep "خَلْفَ" ;
  between_Prep = mkPrep "بَيْنَ" ;
--  both7and_DConj = sd2 "بْته" "َند" ** {n = Pl} ;
--  but_PConj = ss "بُت" ;
  by8agent_Prep = mkPrep "بِ" ;
  by8means_Prep = mkPrep "بِ" ;
  can_VV = mkVV (mkV "طوع" FormX) ;
--  can8know_VV = {
--    s = table VVForm [["بي َبلي تْ"] ; "عَن" ; "عُْلد" ;
--         ["بّن َبلي تْ"] ; ["بِنغ َبلي تْ"] ; "عَنءت" ; "عُْلدنءت"] ;
--    isAux = True
--    } ;
  during_Prep = mkPrep "خِلَالَ" ;
--  either7or_DConj = sd2 "ِتهر" "ْر" ** {n = Sg} ;
  everybody_NP = regNP "الجَمِيع" Pl ;
  every_Det = mkDet "كُلّ" Sg Const ;
  everything_NP = regNP "كُلّ" Sg ;
--  everywhere_Adv = ss "ثريوهري" ;
  few_Det = mkDet "بَعض" Pl Const ;
--  first_Ord = ss "فِرست" ;
  from_Prep = mkPrep "مِنَ" ;
  he_Pron = ResAra.he_Pron ;
  here_Adv = ss "هُنا" ;
--  here7to_Adv = ss ["تْ هري"] ;
--  here7from_Adv = ss ["فرْم هري"] ;
  how_IAdv = ss "كَيفَ" ;
  how8many_IDet = {
    s = \\g,s,c => "كَمْ عَدَد" + caseTbl ! c ;
    n = Pl ; d = Def 
    } ; -- IL

--  if_Subj = ss "ِف" ;
  in8front_Prep = mkPrep "مُقَابِلَ" ;
  i_Pron  = ResAra.i_Pron ;
  in_Prep = mkPrep "فِي" ;
  it_Pron = emptyNP ** {s = \\_ => "هَذَا"} ; -- was: it_Pron = mkPron "ِت" "ِت" "ِتس" (Per3 Masc Sg);
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
  no_Utt = {s = \\_ => "لا"} ;
  on_Prep = mkPrep "عَلى" ;
--- DEPREC  one_Quant = mkQuantNum "واحِد" Sg Indef ;
  only_Predet = mkPredet "فَقَط" False;
--  or_Conj = ss "ْر" ** {n = Sg} ;
--  otherwise_PConj = ss "ْتهروِسي" ;
  part_Prep = mkPrep "مِنَ" ;
--  please_Voc = ss "ةلَسي" ;
  possess_Prep = mkPrep "ل" ;
--  quite_Adv = ss "قُِتي" ;
  she_Pron = ResAra.she_Pron ;
--  so_AdA = ss "سْ" ;
  somebody_NP = regNP "أَحَد" Sg ;
  someSg_Det = mkDet "أَحَد" Sg Const ;
  somePl_Det = mkDet "بَعض" Pl Const ;
  something_NP = regNP "شَيْء" Sg ;
--  somewhere_Adv = ss "سْموهري" ;
  that_Quant = mkQuant3 "ذَلِكَ" "تِلكَ" "أُلٱِكَ" Def;
  that_Subj = ss "أنَّ" ;
----b  that_NP = indeclNP "ذَلِكَ" Sg ;
  there_Adv = ss "هُناك" ;
--  there7to_Adv = ss "تهري" ;
--  there7from_Adv = ss ["فرْم تهري"] ;
--  therefore_PConj = ss "تهرفْري" ;
----b  these_NP = indeclNP "هَؤُلَاء" Pl ;
 they_Pron = theyMasc_Pron ;
  this_Quant = mkQuant7 "هَذا" "هَذِهِ" "هَذَان" "هَذَيْن" "هَاتَان" "هَاتَيْن" "هَؤُلَاء" Def;
----b  this_NP = indeclNP "هَذا" Sg ;
----b  those_NP = indeclNP "هَؤُلَاءكَ" Pl ;
  through_Prep = mkPrep "عَبْرَ" ;
--  too_AdA = ss "تّْ" ;
  to_Prep = mkPrep "إِلى" ;
  under_Prep = mkPrep "تَحْتَ" ;
--  very_AdA = ss "ثري" ;
--  want_VV = P.mkVV (P.regV "وَنت") ;
  we_Pron = ResAra.we_Pron ;
  whatPl_IP = mkIP "ما" "ماذا" Pl ;
  whatSg_IP = mkIP "ما" "ماذا" Sg ;
  when_IAdv = ss "مَتَى" ;
--  when_Subj = ss "وهن" ;
  where_IAdv = ss "أَينَ" ;
  which_IQuant = {
    s = \\s,c => case <c,s> of {
             <Bare,_>    => "أيّ" ;
             <Nom,Indef> => "أيٌّ" ;
             <Nom,_>     => "أيُّ" ;
             <Acc,Indef> => "أيّاً" ;
             <Acc,_>     => "أيَّ" ;
             <Gen,Indef> => "أيٍّ" ;
             <Gen,_>     => "أيِّ"
             }
    } ;
  whoSg_IP = mkIP "مَنْ" "مَنْ" Sg ;
  whoPl_IP = mkIP "مَنْ" "مَنْ" Pl ;
--  why_IAdv = ss "وهي" ;
  without_Prep = mkPrep "بِدُونِ" ;
  with_Prep = mkPrep "مَع" ;
  yes_Utt = {s = \\_ => "نَعَم"} ;
  youSg_Pron = youSgMasc_Pron ;
  youPl_Pron = youPlMasc_Pron ;
  youPol_Pron = youSgFem_Pron ; -- arbitrary?

  have_V2      = dirV2 (regV "يَملِك") ;

  lin language_title_Utt = {s = \\_ => "العربية"} ;

}
