concrete StructuralHrv of Structural = CatHrv **
  open ParadigmsHrv, ResHrv, Prelude in {

lin
    and_Conj = mkConj "i" ;
----    by8agent_Prep = mkPrep "" Ins ; 
----    few_Det = invarNumeral "málo" ; -- see notes
    for_Prep = mkPrep "pre" accusative ;
    from_Prep = mkPrep "iz" Gen ;
    have_V2 = mkV2 (mkV imati_VerbForms) ;
    in_Prep = mkPrep "u" Loc ; 
----    many_Det = regNumeral "mnoho" "mnohých" "mnohým" "mnohými" ; ---- alternative: invarNumeral "veľa" ;
    or_Conj = mkConj "alebo" ;
----    somePl_Det = invarDeterminer "niekoľko" Num5 ;
----    somePl_Det = {s = \\g,c => (demPronFormsAdjective (mkDemPronForms "niekoľko") "").s ! g ! Pl ! c ; size = Num5} ;
----    something_NP = {s,clit,prep = \\c => "nie" + coForms ! c ; a = Ag Neutr Sg P3 ; hasClit = False} ; -- CEG 5.6.3
    possess_Prep = mkPrep "" Gen ;
    that_Quant = adjFormsAdjective (velikA "oni" ** {msnom = "onaj"}) ; ---- TODO: taj, ta, to
    this_Quant = adjFormsAdjective (velikA "ovi" ** {msnom = "ovaj"}) ;
    to_Prep = mkPrep "u" Acc ;
    with_Prep = mkPrep (pre {"s"|"z"|"š"|"ž"|"mnom" => "sa" ; _ => "s"}) Ins ; 

    i_Pron = mkPron (Ag (Masc Anim) Sg P1) ;   --- to add Fem pronouns in Extend
    youSg_Pron = mkPron (Ag (Masc Anim) Sg P2) ;
    he_Pron = mkPron (Ag (Masc Anim) Sg P3) ;
    she_Pron = mkPron (Ag Fem Sg P3) ;
    it_Pron = mkPron (Ag Neutr Sg P3) ;
    we_Pron = mkPron (Ag (Masc Anim) Pl P1) ;
    youPl_Pron = mkPron (Ag (Masc Anim) Pl P2) ;
    they_Pron = mkPron (Ag (Masc Anim) Pl P3) ;

    somewhere_Adv = mkAdv "negdje" ;

    if_Subj = lin Subj {s = "ako"} ;
    every_Det = {s = \\g, c => (adjFormsAdjective (velikA "svaki")).s ! g ! Sg ! c ; size = NS_1} ;
    all_Predet = adjFormsAdjective (velikA "svi") ;
    that_Subj = lin Subj {s = "da"} ;
    someSg_Det = {s = \\g, c => (adjFormsAdjective (velikA "neki")).s ! g ! Sg ! c ; size = NS_1} ;
    at_least_AdN = lin AdN {s = "najmanje"} ;
    part_Prep = mkPrep genitive ;


--    ExtAdvS
--    mkN2
--    mkA2

}
