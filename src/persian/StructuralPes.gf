concrete StructuralPes of Structural = CatPes **
  open MorphoPes, ParadigmsPes, Prelude, NounPes, (R=ResPes) in {

  flags optimize=all ;
  coding = utf8;

  lin
  above_Prep = mkPrep "بالای" ;
  after_Prep = mkPrep ["بعد از"] ;
  all_Predet = ss (zwnj "همه" "ی") ;
  almost_AdA, almost_AdN = ss "تقریباً" ;
  although_Subj = mkSubj "با وجود این" ;
  always_AdV = ss "همیشه" ;
  and_Conj = sd2 [] "و" ** {n = Pl} ;
  because_Subj = mkSubj "برای این" ;
  before_Prep = mkPrep ["قبل از"] ;
  behind_Prep = mkPrep "پشت" ;
  between_Prep = mkPrep "بین" ;
  both7and_DConj = sd2 "هم" ["و هم"] ** {n = Pl} ;
  but_PConj = ss "اما" ;
  by8agent_Prep = mkPrep "توسط" ;
  by8means_Prep = mkPrep "با" ;
--  can8know_VV,can_VV = mkVV (mkV "سکن") ;
  can_VV = let isAux = False in mkVV isAux subjunctive (mkV_1 "توانستن") ; ---- AR
  during_Prep = mkPrep ["در طول"] ;
  either7or_DConj = sd2 "یا" "یا" ** {n = Sg} ;
--  everybody_NP =  R.indeclNP "هر کwی";
  every_Det = mkDet "هر" Sg ;
  everything_NP = DetCN (mkDet "همه" Sg) (UseN (mkN "چیز")) ;
  everywhere_Adv = ss ["هر جا"] ;
  few_Det = mkDet ["تعداد کمی"] Pl True; -- check
  for_Prep = mkPrep "برای" Ezafe ;
  from_Prep = mkPrep "از" ;
  he_Pron = R.agr2pron ! Ag Sg P3 ;
  here_Adv = ss "اینجا" ;
  here7to_Adv = ss "اینجا" ;
  here7from_Adv = ss "اینجا" ;
  how_IAdv = ss "چطور" ;
  how8many_IDet = {s = "چند" ; n = Pl ; isNum = True} ;
  how8much_IAdv  = ss "چقدر" ;
  if_Subj = mkSubj subjunctive "اگر" ;
  in8front_Prep = mkPrep "جلوی" ;
  i_Pron = R.agr2pron ! Ag Sg P1;
  in_Prep = mkPrep "در" ;
  it_Pron  = R.agr2pron ! Ag Sg P3;
  less_CAdv = {s = "کمتر" ; p = ""} ;
  many_Det = let isNum, isNeg = False in  mkDet "بسیار" Pl isNum isNeg Ezafe ;
  more_CAdv = {s = "بیشتر" ; p = "" } ;
  most_Predet = ss "اکثر";
  much_Det = mkDet ["مقدار زیادی"]  Pl ;
  must_VV =
    let must : VV = mkVV False subjunctive (defV "بایستن" "باید" "باید") ; -- "بایست" is different meaning?
     in must ** {isDef=True} ;
   -- TODO: past tense forms with مجبور+beVerb
  no_Utt = ss "نه" ;
  on_Prep = mkPrep "روی" ;
  only_Predet = ss "فقط" ;
  or_Conj = sd2 [] "یا" ** {n = Sg} ;
  otherwise_PConj = ss ["درغیراین صورت"] ;
  part_Prep = mkPrep "از" Ezafe ;
  please_Voc = ss "لطفاً" ;
  possess_Prep = mkPrep [] Ezafe ;
  quite_Adv = ss "کاملاً" ;
  she_Pron = R.agr2pron ! Ag Sg P3 ;
  so_AdA = ss "بسیار" ;
  somebody_NP = R.indeclNP "کwی" ;
  someSg_Det = mkDet "مقداری" Sg True ;
  somePl_Det = mkDet "چند" Pl True ;
  something_NP = R.indeclNP "چیزی" ;
  somewhere_Adv = ss "جایی" ;
  that_Quant = mkQuant "آن" "آن";
  that_Subj = mkSubj "آن";
  there_Adv = ss "آنجا" ;
  there7to_Adv = ss "آنجا" ;
  there7from_Adv = ss "آنجا" ;
  therefore_PConj = ss ["به همین دلیل"] ;
  they_Pron = R.agr2pron ! Ag Pl P3 ;
  this_Quant = mkQuant "این" "این" ;
  through_Prep = mkPrep ["از طریق"] ;
  too_AdA = ss "خیلی" ;
  to_Prep = mkPrep "به" ** {lock_Prep = <>};
  under_Prep = mkPrep "زیر" ** {lock_Prep = <>};
  very_AdA = ss "خیلی" ;
  want_VV = mkVV False subjunctive (mkV "خواستن" "خواه") ; --not aux
  we_Pron = R.agr2pron ! Ag Pl P1 ;
  whatSg_IP = {s = ["چه چیزی"] ; n = Sg } ;
  whatPl_IP = {s = ["چه چیزهایی"] ; n = Pl } ;
  when_IAdv = ss "کی" ;
  when_Subj = mkSubj "وقتی" ;
  where_IAdv = ss "کجا" ;
  which_IQuant = {s = "کدام" ; n = Sg} ;
  whichPl_IDet = {s = "کدام" ; n = Pl ; isNum = False} ;
  whichSg_IDet = { s = "کدام" ; n = Sg ; isNum = False} ;
  whoSg_IP = {s = ["چه کسی"] ; n =  Sg}  ;
  whoPl_IP = {s = ["چه کسانی"] ;n = Pl} ;
  why_IAdv = ss "چرا" ;
  without_Prep = mkPrep "بدون" ;
  with_Prep = mkPrep "با";
--  yes_Phr = ss "بله" ;
  yes_Utt = ss "بله" ;
  youSg_Pron = R.agr2pron ! Ag Sg P2 ;
  youPl_Pron = R.agr2pron ! Ag Pl P2 ;
  youPol_Pron = R.agr2pron ! Ag Pl P2  ;
  no_Quant = mkQuant "هیچ" "هیچ" Clitic True ;
  not_Predet = {s="نه"} ;
  if_then_Conj = sd2 "اگر" "آنگاه" ** {n = Sg} ;
  at_least_AdN = ss "حداقل" ;
  at_most_AdN = ss "حداکثر";
  nothing_NP = R.indeclNP "هیچ چیز" ** {isNeg = True} ;
  except_Prep = mkPrep ["به جز"] ;
  nobody_NP = R.indeclNP "هیچ کس";

  as_CAdv = {s = ["به اندازه ی"] ; p = ""} ;

----  have_V2 = mkV2 (mkV "داشتن" "دار") "را" ;

 language_title_Utt = ss "فارسی" ;

---- AR from Nasrin

-- have_V2: "have" as an independent verb.
-- MorphoPes.haveVerb: "have" as auxiliary.
have_V2 = haveVerb ** {
  s = table {
        VImp Pos Sg => "داشته باش" ;
        VImp Pos Pl => "داشته باشید" ;
        VImp Neg Sg => "نداشته باش" ;
        VImp Neg Pl => "نداشته باشید" ;
        VSubj p agr => "داشته" ++ subjAux p agr ;
        x => haveVerb.s ! x } ;
  c2 = prepOrRa [] -- "را" ;  ---- AR 18/9/2017: usually no ra acc. to Nasrin, but this is tricky
  } ;
}
