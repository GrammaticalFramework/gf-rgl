concrete PhraseCze of Phrase = CatCze ** open Prelude, ResCze in {

lin
    UttS s = {s = s.s} ;
    UttQS q = {s = q.s} ;
    UttIAdv a = a ;
    UttIP ip = {s = ip.s ! Nom} ;
    UttAdv adv = adv ;
    UttCN cn = {s = cn.s ! Sg ! Nom} ;
    UttAP ap = {s = ap.pred ! Ag (Masc Anim) Sg P3} ;
    UttNP np = {s = np.s ! Nom} ;
    UttVP vp = let agr = Ag Neutr Sg P3 in {s = vp.verb.inf ++ vp.clit ! agr ++ vp.compl ! agr} ;

    -- pol.p selects the verb form; empty pol.s retains the Pol constituent.
    -- Without it, parsing "nečti ji" recovers UttImpSg ?1 instead of PNeg.
    UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.p ! Ag (Masc Anim) Sg P2} ;
    UttImpPl pol imp = {s = pol.s ++ imp.s ! pol.p ! Ag (Masc Anim) Pl P2} ;
    UttImpPol pol imp = {s = pol.s ++ imp.s ! pol.p ! AgPol (Masc Anim)} ;

    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;


    NoPConj = {s = []} ;
    PConjConj conj = {s = conj.s2} ;

    NoVoc = {s = []} ;
    VocNP np = {s = np.s ! ResCze.Voc} ;

}
