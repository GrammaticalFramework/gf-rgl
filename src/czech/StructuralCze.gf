concrete StructuralCze of Structural = CatCze **
  open ParadigmsCze, ResCze, Prelude in {

-- a singular determiner inflecting like an adjective, e.g. "každý", "nějaký"
oper
  adjDet : AdjForms -> Determiner = \afs -> {
    s = \\g,c => (adjFormsAdjective afs).s ! g ! Sg ! c ;
    size = Num1
    } ;

lin
    all_Predet = {s = "všechny"} ;
    and_Conj = mkConj "a" ;
    both7and_DConj = {s1 = "jak" ; s2 = "tak"} ;
    between_Prep = mkPrep "mezi" Ins ;
    by8agent_Prep = mkPrep "od" Gen ; ---- TODO this means "from", there might be no good translation
    by8means_Prep = mkPrep "pomocí" Gen ;
    can_VV = {
      inf = "moci" ;
      pressg1 = "mohu" ;
      pressg2 = "můžeš" ;
      pressg3, negpressg3 = "může" ;
      prespl1 = "můžeme" ;
      prespl2 = "můžete" ;
      prespl3 = "mohou" ;
      pastpartsg = "mohl" ;
      pastpartpl = "mohli" ;
      } ;
    either7or_DConj = {s1 = "buď" ; s2 = "nebo"} ;
    every_Det = adjDet (mladyAdjForms "každý") ;
    few_Det = invarNumeral "málo" ; -- CEG 6.8 --- TODO genitive mála
    for_Prep = mkPrep "pro" accusative ;
    if_Subj = {s = "jestliže"} ;
    no_Quant = adjFormsAdjective (mladyAdjForms "žádný") ;
    on_Prep = mkPrep "na" Loc ;
    someSg_Det = adjDet (mladyAdjForms "nějaký") ;
    that_Subj = {s = "že"} ;
    under_Prep = mkPrep "pod" Ins ;
    where_IAdv = {s = "kde"} ;
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
