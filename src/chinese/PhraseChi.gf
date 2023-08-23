concrete PhraseChi of Phrase = CatChi ** open Prelude, ResChi in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ voc.s ++ utt.s} ;

    UttS s = ss (linS s) ;
    UttQS qs = ss (qs.s ! True) ;
    UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.p} ;
    UttImpPl pol imp = {s = pol.s ++ imp.s ! pol.p} ;
    UttImpPol pol imp = {s = pol.s ++ imp.s ! pol.p} ; --- add politeness here?

    UttIP ip = ip ;
    UttIAdv iadv = iadv ;
    UttNP np = ss (linNP np) ;
    UttCN cn = cn ;
    UttAP ap = {s = ap.s!Attr}  ;
    UttCard x = x ;
    UttVP vp = ss (infVP vp) ;
    UttAdv adv = adv ;
    UttInterj i = i ;

    NoPConj = {s = []} ;
    PConjConj conj = ss (conj.s ! CSent).s2 ;

    NoVoc = {s = []} ;
    VocNP np = {s = linNP np ++ chcomma} ; ---- ??

}
