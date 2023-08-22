concrete PhraseTMP of Phrase = CatTMP ** open Prelude, ResTMP in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = s ;
{-
    UttQS qs = qs ;
    UttIAdv iadv = iadv ;
    UttNP np =
    UttIP ip =
    UttImpSg pol imp = { s = pol.s ++ imp.s ! Sg ! pol.p } ;
    UttImpPl pol imp =
    UttImpPol pol imp = {s = pol.s ++ imp.s ! Sg ! pol.p} ;
    UttVP vp = {s = linVP vp} ;
    UttAP ap = { s = ap.s } ;
    UttAdv adv = {s = } ;
    UttCN n = {s = } ;
    UttCard n = {s = } ;
    UttInterj i = i ; -}
    NoPConj = {s = []} ;
--    PConjConj conj = {s = conj.s1 ++ conj.s2 ! …} ;

    NoVoc = {s = []} ;
--    VocNP np = { s = "," ++ np.s ! … } ;

}
