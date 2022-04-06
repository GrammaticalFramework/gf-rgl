concrete PhraseMay of Phrase = CatMay ** open Prelude, ResMay in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = s ;
    UttQS qs = qs ;
    UttIAdv iadv = iadv ;
    UttNP np = {s = np.s ! Bare} ;
    UttIP ip = {s = ip.sp ! NF Sg Bare} ;
{-
    UttImpSg pol imp =
    UttImpPl pol imp =
    UttImpPol = UttImpSg ;

    UttVP vp = {s = } ;
    UttAdv adv = {s = } ;
    UttCN n = {s = } ;
    UttCard n = {s = } ;
    UttAP ap = { s = ap.s ! } ;
    UttInterj i = i ;
-}
    NoPConj = {s = []} ;
--    PConjConj conj = {s = conj.s1 ++ conj.s2 ! …} ;

    NoVoc = {s = []} ;
--    VocNP np = { s = "," ++ np.s ! … } ; -}

}
