concrete PhraseMay of Phrase = CatMay ** open Prelude, ResMay in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = s ;
    UttQS qs = qs ;
    UttIAdv iadv = iadv ;
    UttNP np = {s = np.s ! Bare} ;
    UttIP ip = {s = ip.s ! Bare} ;
    UttImpSg pol imp = {s = pol.s ++ imp.s ! Sg ! pol.p } ;
    UttImpPol pol imp = {s = "tolong" ++ pol.s ++ imp.s ! Sg ! pol.p} ;
    UttImpPl pol imp = {s = pol.s ++ imp.s ! Pl ! pol.p} ;
    UttVP vp = {s = linVP vp} ;
    UttAP ap = {s = ap.s} ;
    UttAdv adv = adv ;
    UttCN n = {s = linCN n} ;
    UttCard c = c ;
    UttInterj i = i ;
    NoPConj = {s = []} ;
    PConjConj conj = {s = conj.s1 ++ conj.s2} ;

    NoVoc = {s = []} ;
    VocNP np = { s = "," ++ np.s ! Bare} ;

}
