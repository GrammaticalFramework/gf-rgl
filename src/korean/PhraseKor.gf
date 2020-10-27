concrete PhraseKor of Phrase = CatKor ** open Prelude, ResKor in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = {s = s.s ! Statement linStyle} ;
    UttQS qs = {s = qs.s ! linStyle} ;
    UttIAdv iadv = iadv ;
    UttAdv adv = adv ;
    UttInterj i = i ;
{-
    UttImpSg pol imp =
    UttImpPl pol imp =
    UttImpPol = UttImpSg ;
    -}
    UttIP ip = {s = ip.s ! Bare} ;

    UttNP np = {s = np.s ! Bare} ;
    UttVP vp = {s = linVP (VF Plain Pos) vp} ;
    UttCN cn = {s = cn.rs ++ cn.s ! Bare} ;
    UttCard n = {s = n.s ! NK ! Indep} ;
    UttAP ap = {s = ap.compar ++ ap.s ! VF Plain Pos} ;

    NoPConj = {s = []} ;
--    PConjConj conj = {s = conj.s1 ++ conj.s2 ! …} ;

    NoVoc = {s = []} ;
--    VocNP np = { s = "," ++ np.s ! … } ; -}

}
