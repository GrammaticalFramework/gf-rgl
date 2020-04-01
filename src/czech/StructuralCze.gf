concrete StructuralCze of Structural = CatCze ** 
  open ParadigmsCze, ResCze, Prelude in {

lin
    and_Conj = mkConj "i" ; 
----    by8agent_Prep : Prep
----    few_Det : Det
    from_Prep = mkPrep (pre {"s"|"z" => "ze" ; _ => "z"}) Gen ; ---- consonant clusters
    have_V2 = mkV2 haveVerbForms ;
    in_Prep = mkPrep (pre {"v"|"m" => "ve" ; _ => "v"}) Loc ; ----
----    many_Det : Det
    or_Conj = mkConj "nebo" ;
----    somePl_Det : Det
----    something_NP : NP
    possess_Prep = mkPrep "" Gen ;
    that_Quant = demPronFormsAdjective (mkDemPronForms "tamt") "" ;
    this_Quant = demPronFormsAdjective (mkDemPronForms "t") "to" ;
    to_Prep = mkPrep "do" Gen ;
    with_Prep = mkPrep (pre {"s"|"z" => "se" ; _ => "s"}) Ins ; ----

    i_Pron = personalPron (Ag (Masc Anim) Sg P1) ;
    youSg_Pron = personalPron (Ag (Masc Anim) Sg P2) ;
    he_Pron = personalPron (Ag (Masc Anim) Sg P3) ;
    she_Pron = personalPron (Ag Fem Sg P3) ;

}
