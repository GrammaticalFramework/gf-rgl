concrete PhraseTel of Phrase = CatTel ** open Prelude, ResTel in {
  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = s ;
    UttQS qs = qs ;
    UttNP np = {s = np.s ! NPC Dir} ;
    UttCN cn = {s = cn.s ! Sg ! Dir} ;
    UttAP ap = {s = ap.s ! Masc ! Sg ! Dir} ;
    UttVP vp = {s = let f = vp.s ! Pos ! VPInf in f.inf ++ f.fin} ;
    UttAdv adv = adv ;
    UttIP ip = {s = ip.s ! Dir} ;
    UttCard card = {s = card.s ! Neutr} ;
    UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.p ! Sg} ;
    UttImpPl pol imp = {s = pol.s ++ imp.s ! pol.p ! Pl} ;
    UttImpPol pol imp = {s = pol.s ++ imp.s ! pol.p ! Pl} ;

    NoPConj = {s = []} ;

    NoVoc = {s = []} ;
    VocNP np = {s = "," ++ np.s ! NPC Dir} ;
}
