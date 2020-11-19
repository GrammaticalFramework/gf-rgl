concrete PhraseHun of Phrase = CatHun ** open Prelude, ResHun in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = s ;
    UttQS qs = qs ;
    UttIAdv iadv = iadv ;
{-
    UttImpSg pol imp =
    UttImpPl pol imp =
    UttImpPol = UttImpSg ;
-}
    UttIP,
    UttNP = \np -> {s = linNP np} ;
    UttVP vp = {s = vp.obj ++ vp.adv ++ vp.s ! VInf} ;
    UttAdv adv = adv ;
    UttCN cn = {s = linCN cn} ;
    UttCard n = {s = n.s ! Indep} ;
    UttAP ap = {s = ap.s ! Sg ! Nom ++ ap.compl ! Sg} ;
    UttInterj i = i ;

    NoPConj = {s = []} ;
--    PConjConj conj = {s = conj.s1 ++ conj.s2 ! …} ;

    NoVoc = {s = []} ;
--    VocNP np = { s = "," ++ np.s ! … } ; -}

}
