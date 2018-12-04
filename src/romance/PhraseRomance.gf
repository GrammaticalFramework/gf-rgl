incomplete concrete PhraseRomance of Phrase =
  CatRomance ** open CommonRomance, ResRomance, Prelude in {

  flags optimize = all_subs ;

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = {s = s.s ! Indic} ;
    UttQS qs = {s = qs.s ! QDir} ;
    UttImpSg pol imp  = {s = pol.s ++ imp.s ! pol.p ! ImpF Sg False ! Fem} ;
    UttImpPl pol imp  = {s = pol.s ++ imp.s ! pol.p ! ImpF Pl False ! Fem} ;
    UttImpPol pol imp = {s = pol.s ++ imp.s ! pol.p ! ImpF Sg True  ! Fem} ;

    UttIP ip = {s = ip.s ! Nom} ; --- Acc also
    UttIAdv iadv = iadv ;
    UttNP np = {s = (np.s ! Nom).ton} ;
    UttVP vp = {s = infVP vp (agrP3 Fem Sg)} ; --- Agr
    UttAdv adv = adv ;
    UttCN n = {s = n.s ! Sg} ;
    UttAP ap = {s = ap.s ! genNum2Aform Masc Sg} ;
    UttCard n = {s = n.s ! Masc} ;
    UttInterj i = i ;

    NoPConj = {s = []} ;
    PConjConj conj = {s = conj.s2} ;

    NoVoc = {s = []} ;
    VocNP np = {s = "," ++ (np.s ! Nom).ton} ;

}
