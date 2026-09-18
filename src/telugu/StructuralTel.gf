concrete StructuralTel of Structural = CatTel **
  open (P = ParadigmsTel), Prelude, ResTel in {

  flags optimize=all ;

  lin
  above_Prep = P.mkPrep "పైన" ;
  after_Prep = P.mkPrep "తర్వాత" ;
  almost_AdA = P.mkAdA "దాదాపు" ;
  almost_AdN = P.mkAdN "దాదాపు" ;
  always_AdV = P.mkAdV "ఎల్లప్పుడూ" ;
  as_CAdv = {s = "అంత" ; p = "గా"} ;
  at_least_AdN = P.mkAdN "కనీసం" ;
  at_most_AdN = P.mkAdN "గరిష్ఠంగా" ;
  before_Prep = P.mkPrep "ముందు" ;
  behind_Prep = P.mkPrep "వెనుక" ;
  between_Prep = P.mkPrep "మధ్య" ;
  all_Predet = {s = "అన్ని"} ;
  although_Subj = {s = "అయినప్పటికీ"} ;
  and_Conj = {s1 = [] ; s2 = "మరియు" ; n = Pl} ;
  because_Subj = {s = "ఎందుకంటే"} ;
  by8agent_Prep = P.mkPrep "చేత" ;
  by8means_Prep = P.mkPrep "ద్వారా" ;
  either7or_DConj = {s1 = "గాని" ; s2 = "లేదా" ; n = Sg} ;
  both7and_DConj = {s1 = "రెండూ" ; s2 = "మరియు" ; n = Pl} ;
  every_Det = {s = \\_,_ => "ప్రతి" ; n = Sg} ;
  during_Prep = P.mkPrep "సమయంలో" ;
  everybody_NP = {s = \\_ => "అందరూ" ; a = Ag Masc Pl P3} ;
  everything_NP = {s = \\_ => "అన్నీ" ; a = Ag Neutr Pl P3} ;
  everywhere_Adv = P.mkAdv "ప్రతిచోటా" ;
  except_Prep = P.mkPrep "తప్ప" ;
  few_Det = {s = \\_,_ => "కొద్దిమంది" ; n = Pl} ;
  for_Prep = P.mkPrep "కోసం" ;
  from_Prep = P.mkPrep "నుండి" ;
  if_Subj = {s = "అయితే"} ;
  how8many_IDet = {s = "ఎన్ని" ; n = Pl} ;
  how8much_IAdv = {s = "ఎంత"} ;
  how_IAdv = {s = "ఎలా"} ;
  it_Pron = {
    s = table {PC Dir => "అది" ; PC Obl => "దాని" ; PC Acc => "దానిని" ; PObj => "దానిని" ; PPoss => "దాని"} ;
    a = Ag Neutr Sg P3
    } ;
  or_Conj = {s1 = [] ; s2 = "లేదా" ; n = Sg} ;
  more_CAdv = {s = "మరింత" ; p = "కంటే"} ;
  most_Predet = {s = "చాలా వరకు"} ;
  no_Quant = {s = \\_,_,_ => "ఏ"} ;
  on_Prep = P.mkPrep "మీద" ;
  only_Predet = {s = "మాత్రమే"} ;
  please_Voc = {s = "దయచేసి"} ;
  she_Pron = {
    s = table {PC Dir => "ఆమె" ; PC Obl => "ఆమె" ; PC Acc => "ఆమెను" ; PObj => "ఆమెను" ; PPoss => "ఆమె"} ;
    a = Ag Fem Sg P3
    } ;
  that_Quant = {s = table {
                      Sg => \\_,_ => "ఆ" ;
                      Pl => \\_,_ => "ఆ"
                    }
               } ;
  that_Subj = {s = "అని"} ;
  they_Pron = personalPronoun P3 Pl ** {a = Ag Masc Pl P3} ;
  this_Quant = {s = table {
                      Sg => \\_,_ => "ఈ" ;
                      Pl => \\_,_ => "ఈ"
                    }
               } ;
  very_AdA = {s = "చాలా"} ;
  somebody_NP = {s = \\_ => "ఎవరో ఒకరు" ; a = Ag Masc Sg P3} ;
  something_NP = {s = \\_ => "ఏదో ఒకటి" ; a = Ag Neutr Sg P3} ;
  somewhere_Adv = P.mkAdv "ఎక్కడో" ;
  through_Prep = P.mkPrep "ద్వారా" ;
  to_Prep = P.mkPrep "కు" ;
  too_AdA = P.mkAdA "మరీ" ;
  under_Prep = P.mkPrep "కింద" ;
  want_VV = P.mkVV (P.mkV "కోరు") ;
  whatPl_IP = {s = \\_ => "ఏవి" ; n = Pl} ;
  whatSg_IP = {s = \\_ => "ఏది" ; n = Sg} ;
  when_IAdv = {s = "ఎప్పుడు"} ;
  where_IAdv = {s = "ఎక్కడ"} ;
  which_IQuant = {s = \\_ => "ఏ"} ;
  whoPl_IP = {s = \\c => case c of {Dir => "ఎవరు" ; Obl => "ఎవరి" ; Acc => "ఎవరిని"} ; n = Pl} ;
  whoSg_IP = {s = \\c => case c of {Dir => "ఎవరు" ; Obl => "ఎవరి" ; Acc => "ఎవరిని"} ; n = Sg} ;
  why_IAdv = {s = "ఎందుకు"} ;
  without_Prep = P.mkPrep "లేకుండా" ;
  when_Subj = {s = "అప్పుడు"} ;
  with_Prep = {s = "తో"} ;
  youSg_Pron = personalPronoun P2 Sg ** {a = Ag Masc Sg P2} ;
  youPl_Pron = personalPronoun P2 Pl ** {a = Ag Masc Pl P2} ;
  youPol_Pron = personalPronoun P2 Pl ** {a = Ag Masc Pl P2} ;
  he_Pron = personalPronoun P3 Sg ** {a = Ag Masc Sg P3} ;
  i_Pron = personalPronoun P1 Sg ** {a = Ag Masc Sg P1} ;
  in_Prep = ss "లో" ;
  we_Pron = personalPronoun P1 Pl ** {a = Ag Masc Pl P1} ;
}
