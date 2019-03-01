concrete PhrasePes of Phrase = CatPes ** open Prelude, ResPes in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;
    UttS s = {s = s.s ! Indic} ;
    UttQS qs = {s = qs.s ! QDir} ;
    UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.p ! ImpF Sg False} ;
    UttImpPl pol imp = {s = pol.s ++ imp.s ! pol.p ! ImpF Pl False} ;
    UttImpPol pol imp = {s = pol.s ++ imp.s ! pol.p ! ImpF Sg True} ;

    UttIP, --- Acc also
    UttAdv,
    UttIAdv,
    UttCard = \ss -> ss ;

    UttNP np = {s = np2str np} ;
  	UttCN cn = {s = cn2str cn};
    UttAP ap = {s = ap.s ! Bare} ;
    UttVP vp = {s = showVPH Inf defaultAgr vp} ;

    PConjConj conj = {s = conj.s2} ;

    NoVoc,
    NoPConj = {s = []} ;
    VocNP = UttNP ;
}
