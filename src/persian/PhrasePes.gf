concrete PhrasePes of Phrase = CatPes ** open Prelude, ResPes in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;
    UttS s = {s = s.s ! Indic} ;
    UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.p ! Sg} ;
    UttImpPl pol imp = {s = pol.s ++ imp.s ! pol.p ! Pl} ;
    UttImpPol pol imp = {s = pol.s ++ imp.s ! pol.p ! Pl} ;

    UttIP, --- Acc also
    UttQS,
    UttAdv,
    UttIAdv,
    UttCard = \ss -> ss ;

    UttNP np = {s = np2str np} ;
  	UttCN cn = {s = cn2str cn};
    UttAP ap = {s = ap.s ! Bare} ;
    UttVP vp = {s = infVP vp} ;

    PConjConj conj = {s = conj.s2} ;

    NoVoc,
    NoPConj = {s = []} ;
    VocNP = UttNP ;
}
