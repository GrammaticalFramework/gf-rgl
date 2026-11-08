concrete PhraseKaz of Phrase = CatKaz ** open Prelude, ResKaz in {

  lin
    PhrUtt pc u voc = {s=pc.s ++ u.s ++ voc.s} ;
    UttS s = s ; UttQS s = s ; UttNP np = {s=np.s ! Nom} ; UttCN cn = {s=cn.s ! Nom ! Sg} ;
    UttAP ap = ap ; UttAdv adv = adv ; UttIP ip = ip ; UttInterj i = i ;
    UttImpSg pol imp = {s=imp.s ! rglPolarity pol.p ! Informal ! Sg} ;
    UttImpPl pol imp = {s=imp.s ! rglPolarity pol.p ! Informal ! Pl} ;
    UttImpPol pol imp = {s=imp.s ! rglPolarity pol.p ! Formal ! Sg} ;
    UttVP vp = {s=vp.infinitive} ;
    VocNP np = {s=np.s ! Nom} ; NoVoc = {s=[]} ; NoPConj = {s=[]} ;
}
