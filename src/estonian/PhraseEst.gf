concrete PhraseEst of Phrase = CatEst ** open ResEst, (P = Prelude) in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = s ;
    UttQS qs = {s = qs.s} ;
    UttImpSg  pol imp = {s = pol.s ++ imp.s ! pol.p ! Ag Sg P2} ;
    UttImpPl  pol imp = {s = pol.s ++ imp.s ! pol.p ! Ag Pl P2} ;
    UttImpPol pol imp = {s = pol.s ++ imp.s ! pol.p ! AgPol} ;

    UttIP ip = {s = linIP (NPCase Nom) ip} ;
    UttIAdv iadv = iadv ;
    UttNP np = {s = linNP (NPCase Nom) np} ;
    UttVP vp = {s = infVP (NPCase Nom) Pos (agrP3 Sg) vp InfMa} ;
    UttAdv adv = adv ;
    UttCN cn = {s = linCN (NCase Sg Nom) cn} ;
    UttAP np = {s = np.s ! P.False ! NCase Sg Nom} ;
    UttCard n = {s = n.s ! Sg ! Nom} ;
    UttInterj i = i ;

    NoPConj = {s = []} ;
    PConjConj conj = {s = conj.s2} ;

    NoVoc = {s = []} ;
    VocNP np = {s = "," ++ linNP (NPCase Nom) np} ;

}
