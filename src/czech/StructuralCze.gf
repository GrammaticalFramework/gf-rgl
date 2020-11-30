concrete StructuralCze of Structural = CatCze **
  open ParadigmsCze, ResCze, Prelude in {

lin
    and_Conj = mkConj "a" ;
    by8agent_Prep = mkPrep "od" Gen ; ---- TODO this means "from", there might be no good translation
    few_Det = invarNumeral "málo" ; -- CEG 6.8 --- TODO genitive mála
    for_Prep = mkPrep "pro" accusative ;
    from_Prep = mkPrep (pre {"s"|"z" => "ze" ; _ => "z"}) Gen ; ---- consonant clusters
    have_V2 = mkV2 haveVerbForms ;
    in_Prep = mkPrep (pre {"v"|"m" => "ve" ; _ => "v"}) Loc ; ----
    many_Det = regNumeral "mnoho" "mnoha" ; -- CEG 6.8 ----
    or_Conj = mkConj "nebo" ;
    somePl_Det = regNumeral "několik" "několika" ; -- CEG 6.8 ----
    something_NP = {s,clit,prep = \\c => "ně" + coForms ! c ; a = Ag Neutr Sg P3 ; hasClit = False} ; -- CEG 5.6.3
    possess_Prep = mkPrep "" Gen ;
    that_Quant = demPronFormsAdjective (mkDemPronForms "tamt") "" ;
    this_Quant = demPronFormsAdjective (mkDemPronForms "t") "to" ;
    to_Prep = mkPrep "do" Gen ;
    with_Prep = mkPrep (pre {"s"|"z" => "se" ; _ => "s"}) Ins ; ----

    i_Pron = mkPron (Ag (Masc Anim) Sg P1) ;   --- to add Fem pronouns in Extend
    youSg_Pron = mkPron (Ag (Masc Anim) Sg P2) ;
    he_Pron = mkPron (Ag (Masc Anim) Sg P3) ;
    she_Pron = mkPron (Ag Fem Sg P3) ;
    it_Pron = mkPron (Ag Neutr Sg P3) ;
    we_Pron = mkPron (Ag (Masc Anim) Pl P1) ;
    youPl_Pron = mkPron (Ag (Masc Anim) Pl P2) ;
    they_Pron = mkPron (Ag (Masc Anim) Pl P3) ;

}
