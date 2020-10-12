concrete StructuralSlo of Structural = CatSlo **
  open ParadigmsSlo, ResSlo, Prelude in {

lin
    and_Conj = mkConj "a" ;
    by8agent_Prep = mkPrep "" Ins ; 
    few_Det = invarNumeral "málo" ; -- see notes
    for_Prep = mkPrep "pre" accusative ;
    from_Prep = mkPrep (pre {"z" => "zo" ; _ => "z"}) Gen ; ---- consonant clusters and syllable with the onset with the same place of articulation 
    have_V2 = mkV2 haveVerbForms ;
    in_Prep = mkPrep (pre {"v" => "vo" ; _ => "v"}) Loc ; ----
    many_Det = regNumeral "mnoho" "mnohých" "mnohým" "mnohými" ; ---- alternative: invarNumeral "veľa" ;
    or_Conj = mkConj "alebo" ;
    somePl_Det = invarDeterminer "niekoľko" Num5 ;
---    somePl_Det = {s = \\g,c => (demPronFormsAdjective (mkDemPronForms "niekoľko") "").s ! g ! Pl ! c ; size = Num5} ;
    something_NP = {s,clit,prep = \\c => "nie" + coForms ! c ; a = Ag Neutr Sg P3 ; hasClit = False} ; -- CEG 5.6.3
    possess_Prep = mkPrep "" Gen ;
    that_Quant = demPronFormsAdjective (tenDemPronForms "") "" ;
    this_Quant = demPronFormsAdjective (tenDemPronForms "" ** {msgen = "toh"}) "to" ;
    to_Prep = mkPrep "do" Gen ;
    with_Prep = mkPrep (pre {"s" => "so" ; _ => "s"}) Ins ; 

    i_Pron = personalPron (Ag (Masc Anim) Sg P1) ;
    youSg_Pron = personalPron (Ag (Masc Anim) Sg P2) ;
    he_Pron = personalPron (Ag (Masc Anim) Sg P3) ;
    she_Pron = personalPron (Ag Fem Sg P3) ;

}
