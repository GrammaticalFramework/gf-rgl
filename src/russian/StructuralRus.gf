concrete StructuralRus of Structural = CatRus **
  open ParadigmsRus, ResRus, MorphoRus, (X = ConstructX), Coordination, Prelude in {

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

  which_IQuant = (adjFormsAdjective (makeAdjectiveForms "который" "" "1a" PrefFull)) ** {
    preferShort=PrefFull ;
    g=Neut ;
    c=Nom
    } ;
  this_Quant = (adjFormsAdjective this_forms) ** {
    preferShort=PrefFull ;
    g=Neut ;
    c=Nom
    } ;
  that_Quant = (adjFormsAdjective that_forms) ** {
    preferShort=PrefFull ;
    g=Neut ;
    c=Nom
    } ;
  no_Quant = (adjFormsAdjective (makeAdjectiveForms "никакой" "" "3b" PrefFull)) ** {
    preferShort=PrefFull ;
    g=Neut ;
    c=Nom
    } ;

  above_Prep = {s="над" ; c=Ins; hasPrep=True} ;
  after_Prep = {s="после" ; c=Gen; hasPrep=True };
  before_Prep = {s="перед" ; c=Ins; hasPrep=True};
  behind_Prep = {s="за" ; c=Ins; hasPrep=True };
  between_Prep = {s="между" ; c=Ins; hasPrep=True};
  by8agent_Prep = {s=["с помощью"] ; c=Gen; hasPrep=True};
  by8means_Prep = {s=["с помощью"] ; c=Gen; hasPrep=True};
  during_Prep = {s=["в течение"] ; c=Gen; hasPrep=True};
  except_Prep = {s=["за исключением"] ; c=Gen; hasPrep=True};
  for_Prep = {s="для" ; c=Gen ; hasPrep=True};
  from_Prep = {s="от" ; c=Gen ; hasPrep=True} ;
  in8front_Prep = {s="перед" ; c=Ins; hasPrep=True};
  in_Prep = {s="в" ; c=Loc ; hasPrep=True} ;
  on_Prep = {s="на" ; c=Loc ; hasPrep=True};
  part_Prep = {s="" ; c=Nom ; hasPrep=False }; -- missing in Russian???
  possess_Prep = {s="" ; c=Gen ; hasPrep=False};
  through_Prep = {s="через" ; c=Acc ; hasPrep=True};
  to_Prep = {s="к" ; c=Dat ; hasPrep=True} ;
  under_Prep = {s="под" ; c=Ins ; hasPrep=True};
  without_Prep = {s="без" ; c=Gen; hasPrep=True };

  or_Conj = mkConj "или" Sg ;
  and_Conj = mkConj "и" Pl ;
  both7and_DConj = mkConj "как" (comma ++ "так и") Pl ;
  either7or_DConj = mkConj "либо" (comma ++ "либо") Sg ;
  if_then_Conj = mkConj "если" (comma ++ "то") Sg ;

  with_Prep = {s="с" ; c=Ins; hasPrep=True};

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

  how_IAdv  = ss "как" ;
  how8much_IAdv   = ss "сколько" ;
  when_IAdv = ss "когда" ;
  where_IAdv  = ss "где" ;
  why_IAdv  = ss "почему" ;

  so_AdA = ss "так";

  less_CAdv = X.mkCAdv "менее" "чем" ;
  more_CAdv = X.mkCAdv "более" "чем" ;
  as_CAdv = X.mkCAdv "так же" "как и" ;

  can8know_VV = {v=can; modal=\\a=>[]} ;
  can_VV = {v=can; modal=\\a=>[]} ;
  must_VV = {v=nullVerb; modal=adjFormsToShort (makeAdjectiveForms "должный" "" "1*a" PrefFull)} ;
  want_VV = {v=want; modal=\\a=>[]} ;

  every_Det = {
    s = \\g => (adjFormsAdjective (makeAdjectiveForms "каждый" "" "1*a" PrefFull)).s ! GSg g;
    g = Masc ;
    c = Nom ;
    size = Num1 ;
    } ;
  someSg_Det   = {
    s = \\g => (adjFormsAdjective (makeAdjectiveForms "некоторый" "" "1*a" PrefFull)).s ! GSg g;
    g = Masc ;
    c = Nom ;
    size = Num1 ;
    } ;
  somePl_Det = {
    s = \\g => (adjFormsAdjective (makeAdjectiveForms "некоторый" "" "1*a" PrefFull)).s ! GPl;
    g = Masc ;
    c = Nom ;
    size = NumAll ;
    } ;

  few_Det = { -- numeral! TODO: мало ? немного ?
    s = \\g => (adjFormsAdjective (makeAdjectiveForms "немногий"  "" "3a" PrefFull)).s ! GPl;
    g = Masc ;
    c = Nom ;
    size = NumAll ;
    } ;

  many_Det, much_Det = {
    s = \\g => (adjFormsAdjective (makeAdjectiveForms "многий"  "" "3a" PrefFull)).s ! GPl;
    g = Neut ;
    c = Gen ;
    size = NumAll
    } ;

  only_Predet = (adjFormsAdjective (noShorts only_Pron)) ** {size=Num1} ;
  most_Predet = (makeAdjectiveFromNoun (nounFormsNoun (guessNounForms "большинство"))) ** {size=Num5} ;
  all_Predet = (adjFormsAdjective (noShorts all_Pron)) ** {size=NumAll};
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
    size=NumAll ;    --it depends???
    c=Nom    --???
    } ;

  almost_AdA = ss "почти" ;
  almost_AdN = ss "почти" ;
  always_AdV = ss "всегда" ;
  at_least_AdN = ss "по меньшей мере" ; -- TODO: ?
  at_most_AdN = ss "самое большее" ; -- TODO: ?
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