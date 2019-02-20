concrete PhrasePes of Phrase = CatPes ** open Prelude, ResPes in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = s ;
    UttQS qs = {s = qs.s ! QDir} ;
    UttImpSg pol imp = {s = pol.s ++ imp.s ! contrNeg True pol.p ! ImpF Sg False} ;
    UttImpPl pol imp = {s = pol.s ++ imp.s ! contrNeg True pol.p ! ImpF Pl False} ;
    UttImpPol pol imp = {s = pol.s ++ imp.s ! contrNeg True pol.p ! ImpF Sg True} ;

    UttIP ip = {s = ip.s } ; --- Acc also
    UttIAdv iadv = iadv ;
    UttNP np = {s = np.s !  Bare} ;
    UttVP vp = {s = vp.ad ++ vp.comp ! Ag Sg P3 ++ vp.obj.s ++ vp.inf ++ vp.vComp ! Ag Sg P3 ++ vp.embComp} ;
    UttAdv adv = {s = adv.s } ;
	UttCN cn = {s = cn.s ! Sg ! Bare };
    UttCard n = n ;
    UttAP ap = {s = ap.s ! Bare} ;

    NoPConj = {s = []} ;
    PConjConj conj = {s = conj.s2} ; ---

    NoVoc = {s = []} ;
    VocNP np = {s = np.s !  Bare} ;

}
