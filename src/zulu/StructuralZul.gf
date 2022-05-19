concrete StructuralZul of Structural = CatZul **
  open
    -- MorphoZul,
    ResZul, ParadigmsZul, Prelude, ParamX in {

  flags optimize=all ;

  lin
  -- AdA
  --   almost_AdA = mkAdA "almost" ;
  --   so_AdA = mkAdA "so" ;
  --   too_AdA = mkAdA "too" ;
    very_AdA = mkAdA "kakhulu" ;

  -- AdN
  --   almost_AdN = mkAdN "almost" ;
  --   at_least_AdN = mkAdN "at least" ;
  --   at_most_AdN = mkAdN "at most" ;

  -- Adv
  --   everywhere_Adv = mkAdv "everywhere" ;
  --   here_Adv = mkAdv "here" ;
  --   here7to_Adv = mkAdv ["to here"] ;
  --   here7from_Adv = mkAdv ["from here"] ;
  --   quite_Adv = mkAdv "quite" ;
  --   somewhere_Adv = mkAdv "somewhere" ;
  --   there_Adv = mkAdv "there" ;
  --   there7to_Adv = mkAdv "there" ;
  --   there7from_Adv = mkAdv ["from there"] ;

  -- AdV
  --   always_AdV = mkAdV "always" ;

  -- CAdv
  --   less_CAdv = mkCAdv "less" "no less" "than" ;
  --   more_CAdv = mkCAdv "more" "no more" "than" ;
  --   as_CAdv = mkCAdv "as" "not as" "as" ;

  -- Conj
  and_Conj = { s = \\_ => "futhi" ; fix = False } ;
  --   both7and_DConj = mkConj "both" "and";
  --   either7or_DConj = mkConj "either" "or" singular ;
  --   or_Conj = mkConj "or" singular ;
  --   if_then_Conj = mkConj "if" "then" singular ;

  -- Det
  --   every_Det = mkDeterminerSpec singular "every" "everyone" "everything" False ;
  --   few_Det = mkDeterminer plural "few" ;
  --   many_Det = mkDeterminer plural "many" ;
  --   much_Det = mkDeterminer singular "much" ;
  --   someSg_Det = mkDeterminer singular "some" ;
  --   somePl_Det = mkDeterminer plural "some" ;

  -- IAdv
    how_IAdv = {s = "kanjani" ; postIAdv = False } ;
    how8much_IAdv = {s = "kangakanani" ; postIAdv = False } ;
  --   when_IAdv = ss "when" ;
  --   where_IAdv = ss "where" ;
  --   why_IAdv = ss "why" ;

  -- IDet
  --   how8many_IDet = mkDeterminer plural ["how many"] ;
  --   whichPl_IDet = mkDeterminer plural ["which"] ;
  --   whichSg_IDet = mkDeterminer singular ["which"] ;

  -- IP
  --   whatPl_IP = mkIP "what" "what" "what's" plural ;
  --   whatSg_IP = mkIP "what" "what" "what's" singular ;
  --   whoPl_IP = mkIP "who" "whom" "whose" plural ;
  --   whoSg_IP = mkIP "who" "whom" "whose" singular ;

  -- IQuant
  --   which_IQuant = {s = \\_ => "which"} ;

  -- NP
  --   everybody_NP = regNP "everybody" singular ;
  --   everything_NP = regNP "everything" singular ;
  --   somebody_NP = regNP "somebody" singular ;
  --   something_NP = regNP "something" singular ;
  --   nobody_NP = regNP "nobody" singular ;
  --   nothing_NP = regNP "nothing" singular ;

  -- PConj
  --   but_PConj = ss "but" ;
  --   otherwise_PConj = ss "otherwise" ;
  --   therefore_PConj = ss "therefore" ;

  -- Predet
    -- all_Predet = { s = "nke" ; isPost = True } ;
  --   most_Predet = ss "most" ;
    -- only_Predet = { s = "dwa" ; isPost = True } ;
    -- not_Predet = { s : Str ; n : Number ; isPost : Bool }

  -- Prep
  --   above_Prep = mkPrep "above" ;
  --   after_Prep = mkPrep "after" ;
  --   before_Prep = mkPrep "before" ;
  --   behind_Prep = mkPrep "behind" ;
  --   between_Prep = mkPrep "between" ;
  --   by8agent_Prep = mkPrep "by" ;
  --   by8means_Prep = mkPrep "by" ;
  --   during_Prep = mkPrep "during" ;
  --   for_Prep = mkPrep "for" ;
  --   from_Prep = mkPrep "from" ;
  --   in8front_Prep = mkPrep ["in front of"] ;
  --   in_Prep = mkPrep "in" ;
  --   on_Prep = mkPrep "on" ;
  --   part_Prep = mkPrep "of" ;
  --   possess_Prep = mkPrep "of" ;
  --   through_Prep = mkPrep "through" ;
  --   to_Prep = mkPrep "to" ;
  --   under_Prep = mkPrep "under" ;
  --   without_Prep = mkPrep "without" ;
  --   with_Prep = mkPrep "with" ;
  --   except_Prep = mkPrep "except" ;

  -- Pron
    i_Pron  = mkPron (First Sg) ;
    we_Pron = mkPron (First Pl) ;

    youSg_Pron = mkPron (Second Sg) ;
    youPl_Pron = mkPron (Second Pl) ;

    he_Pron = mkPron (Third C1_2 Sg) ;
    it_Pron = mkPron (Third C3_4 Sg) ;
    she_Pron = mkPron (Third C1_2 Sg) ;
    they_Pron = mkPron (Third C1_2 Pl) ;

    --   youPol_Pron = mkPron "you" "you" "your" "yours" singular P2 human ;

  -- Quant (we use this category for demonstratives only)
    that_Quant = { s = [] ; dist = Dem2 } ;
    this_Quant = { s = [] ; dist = Dem1 } ;
  --   no_Quant = mkQuant "no" "no" "none" "none" ;

  -- Subj
  --   although_Subj = ss "although" ;
  --   because_Subj = ss "because" ;
  --   if_Subj = ss "if" ;
  --   when_Subj = ss "when" ;
  --   that_Subj = ss "that" ;

  -- Utt
  --   no_Utt = ss "no" ;
  --   yes_Utt = ss "yes" ;
    language_title_Utt = ss "Zulu" ;

  -- Voc
  --   please_Voc = ss "please" ;

  -- V2
  --   have_V2 = dirV2 (mk5V "have" "has" "had" "had" "having") ;

  -- VV
  --   can8know_VV
  --   can_VV
  --   must_VV

}
