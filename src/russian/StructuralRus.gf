concrete StructuralRus of Structural = CatRus **
  open ParadigmsRus, ResRus, MorphoRus, ParamRus, Maybe, (X = ConstructX), Coordination, Prelude in {

lin
  i_Pron = personalPron (Ag MSg P1) ;
  we_Pron = personalPron (Ag GPl P1) ;
  youSg_Pron = personalPron (Ag MSg P2) ;
  youPl_Pron = personalPron (Ag GPl P2) ;
  youPol_Pron = youPl_Pron ;
  he_Pron = personalPron (Ag MSg P3) ;
  she_Pron = personalPron (Ag FSg P3) ;
  it_Pron = personalPron (Ag NSg P3) ;
  they_Pron = personalPron (Ag GPl P3) ;

  whatSg_IP = what_sg ;
  whatPl_IP = what_pl ;
  whoSg_IP = who_sg ;
  whoPl_IP = who_pl ;

  -- : IQuant ;
  which_IQuant = (adjFormsAdjective (makeAdjectiveForms "который" "" "1a" PreferFull)) ** {
    preferShort=PreferFull ;
    g=Neut ;
    c=Nom
    } ;
  -- : Quant ;
  this_Quant = (adjFormsAdjective this_forms) ** {
    type=NormalDet ;
    preferShort=PreferFull ;
    c=Nom
    } ;
  -- : Quant ;
  that_Quant = (adjFormsAdjective that_forms) ** {
    type=NormalDet ;
    preferShort=PreferFull ;
    c=Nom
    } ;
  -- : Quant ;
  no_Quant = (adjFormsAdjective (makeAdjectiveForms "никакой" "" "3b" PreferFull)) ** {
    type=NormalDet ;
    preferShort=PreferFull ;
    c=Nom
    } ;

  above_Prep = mkPrep above_prep_ins_mod Ins ;
  after_Prep = mkPrep "после" Gen ;
  before_Prep = mkPrep "перед" Ins ;
  behind_Prep = mkPrep "за" Ins ;
  between_Prep = mkPrep "между" Ins ;
  by8agent_Prep = mkPrep ["с помощью"] Gen ;
  by8means_Prep = mkPrep ["с помощью"] Gen ;
  during_Prep = mkPrep "в течение" Gen ;
  except_Prep = mkPrep ["за исключением"] Gen ; -- fix n
  for_Prep = mkPrep "для" Gen ;
  from_Prep = mkPrep ot_prep_gen_mod Gen ;
  in8front_Prep = mkPrep "перед" Ins ;
  in_Prep = mkPrep v_prep_mod Loc ;
  on_Prep = mkPrep "на" Loc ;
  part_Prep = {s="" ; c=Ptv ; neggen=False ; hasPrep=False } ;
  possess_Prep = {s="у" ; c=Gen ; neggen=False ; hasPrep=False} ;
  through_Prep = mkPrep "через" Acc ;
  to_Prep = mkPrep k_prep_dat_mod Dat ;
  under_Prep = mkPrep pod_prep_mod Ins ;
  without_Prep = mkPrep "без" Gen ;

  or_Conj = mkConj "или" Sg ;
  and_Conj = mkConj "и" Pl ;
  both7and_DConj = mkConj "как" (comma ++ "так и") Pl ;
  either7or_DConj = mkConj "либо" (comma ++ "либо") Sg ;
  if_then_Conj = mkConj "если" (comma ++ "то") Sg ;

  with_Prep = mkPrep s_prep_mod Ins ;

  please_Voc = ss "пожалуйста" ;

  everywhere_Adv = mkAdv "везде" ;
  here_Adv = mkAdv "здесь" ;
  here7to_Adv = mkAdv "сюда" ;
  here7from_Adv = mkAdv "отсюда" ;
  quite_Adv = mkAdv "довольно" ;
  somewhere_Adv  = mkAdv "где-нибудь" ;
  there_Adv = mkAdv "там" ;
  there7to_Adv = mkAdv "туда" ;
  there7from_Adv = mkAdv "оттуда" ;
  always_AdV = mkAdV "всегда";
  --always_AdV = {s="всегда"; p=Pos} ;

  how_IAdv = ss "как" ;
  how8much_IAdv = ss "сколько" ;
  when_IAdv = ss "когда" ;
  where_IAdv = ss "где" ;
  why_IAdv = ss "почему" ;

  so_AdA = ss "так";

  less_CAdv = X.mkCAdv "менее" "чем" ;
  more_CAdv = X.mkCAdv "более" "чем" ;
  as_CAdv = X.mkCAdv "так же" "как и" ;  -- requires short form adjective

  can8know_VV = {v=can; modal=\\a=>[]} ;
  can_VV = {v=can; modal=\\a=>[]} ;
  must_VV = {v=nullVerb; modal=adjFormsToShort (makeAdjectiveForms "должный" "" "1*a" PreferFull)} ;
  want_VV = {v=want; modal=\\a=>[]} ;

  -- : Det ;
  every_Det = {
    s = \\g => (adjFormsAdjective (makeAdjectiveForms "каждый" "" "1*a" PreferFull)).s ! GSg g;
    type=NormalDet ;
    g = Masc ;
    c = Nom ;
    size = Num1 ;
    } ;
  -- : Det ;
  someSg_Det   = {
    s = \\g => (adjFormsAdjective (makeAdjectiveForms "некоторый" "" "1*a" PreferFull)).s ! GSg g;
    type=NormalDet ;
    g = Masc ;
    c = Nom ;
    size = Num1 ;
    } ;
  -- : Det ;
  somePl_Det = {
    s = \\g => (adjFormsAdjective (makeAdjectiveForms "некоторый" "" "1*a" PreferFull)).s ! GPl;
    type=NormalDet ;
    g = Masc ;
    c = Nom ;
    size = NumAll ;
    } ;
  -- : Det ;
  few_Det = {
    s = \\g => (adjFormsAdjective (makeAdjectiveForms "немногий"  "" "3a" PreferFull)).s ! GPl;
    type=NormalDet ;
    g = Masc ;
    c = Nom ;
    size = NumAll ;
    } ;
  -- : Det ;
  many_Det, much_Det = {
    s = \\g => (adjFormsAdjective (makeAdjectiveForms "многий"  "" "3a" PreferFull)).s ! GPl;
    type=NormalDet ;
    g = Neut ;
    c = Gen ;
    size = NumAll
    } ;

  only_Predet = (adjFormsAdjective (pronToAdj only_Pron)) ** {size=Num1} ;
  most_Predet = (makeAdjectiveFromNoun (nounFormsNoun (guessNounForms "большинство" (guessAdjectiveForms "") GenType))) ** {size=Num5} ;
  all_Predet = (adjFormsAdjective (pronToAdj all_Pron)) ** {size=NumAll};
  not_Predet = (adjFormsAdjective (mkA "не" "" "0")) ** {size=Num1} ;

  how8many_IDet = {
    s=\\g,anim,cas => case <anim,cas> of {
      <_,Nom|VocRus|Ptv> => "сколько" ;
      <_,Gen|Pre|Loc> => "скольких" ;
      <_,Dat> => "скольким" ;
      <_,Ins> => "сколькими" ;
      <Inanimate,Acc> => "сколько" ;
      <Animate,Acc> => "скольких"   -- also as Nom?
      } ;
    g=Neut ;
    size=Num5 ;    --it depends???
    c=Gen    --???
    } ;

  almost_AdA = ss "почти" ;
  almost_AdN = ss "почти" ;
  at_least_AdN = ss "по меньшей мере" ;
  at_most_AdN = ss "самое большее" ;
  too_AdA = ss "слишком" ;
  very_AdA = ss "очень" ;

  everybody_NP = lin NP everybody ;
  everything_NP = lin NP everything ;
  something_NP = lin NP something ;
  somebody_NP = lin NP somebody ;
  nothing_NP = lin NP nothing ;
  nobody_NP = lin NP nobody ;

  but_PConj = ss "но" ;
  otherwise_PConj = ss "иначе" ;
  therefore_PConj = ss "следовательно" ;

  although_Subj = ss "хотя" ;
  because_Subj = ss ["потому что"] ;
  if_Subj = ss "если" ;
  when_Subj = ss "когда" ;
  that_Subj = ss "что" ;  -- TODO: ?

  have_V2 = dirV2 (mkV Imperfective "иметь" "имею" "имеет");

  language_title_Utt = ss "русский" ;
  yes_Utt = ss ["да"] ;
  no_Utt  = ss ["нет"] ;
}
