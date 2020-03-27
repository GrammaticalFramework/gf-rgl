concrete PhraseKor of Phrase = CatKor ** open Prelude, ResKor in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = {s = s.s ! Statement} ;
    UttQS qs = qs ;
    UttIAdv iadv = iadv ;
{-
    UttImpSg pol imp =
    UttImpPl pol imp =
    UttImpPol = UttImpSg ;

    UttIP ip = {s = ip.s ! } ;
    UttNP np = {s = np.s ! } ;
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
