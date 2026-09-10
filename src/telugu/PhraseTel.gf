concrete PhraseTel of Phrase = CatTel ** open Prelude, ResTel in {
--
  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = s ;
    UttQS qs = qs ;
    UttNP np = {s = np.s ! NPC Dir} ;
    UttCN cn = {s = cn.s ! Sg ! Dir} ;
    UttAP ap = {s = ap.s ! Masc ! Sg ! Dir} ;
    UttVP vp = {s = let f = vp.s ! Pos ! VPInf in f.inf ++ f.fin} ;
    UttAdv adv = adv ;
    UttImpSg pol imp = {s = pol.s ++ imp.s} ;
    UttImpPl pol imp = {s = pol.s ++ imp.s} ;
    UttImpPol pol imp = {s = pol.s ++ imp.s} ;

    NoPConj = {s = []} ;
--    PConjConj conj = {s = conj.s2} ; ---
--
    NoVoc = {s = []} ;
--    VocNP np = {s = "," ++ np.s ! Nom} ;
--
}
