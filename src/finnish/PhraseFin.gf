concrete PhraseFin of Phrase = CatFin ** open ResFin, StemFin, (P = Prelude) in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = s ;
    UttQS qs = {s = qs.s} ;
    UttImpSg  pol imp = {s = pol.s ++ imp.s ! pol.p ! Ag Sg P2} ;
    UttImpPl  pol imp = {s = pol.s ++ imp.s ! pol.p ! Ag Pl P2} ;
    UttImpPol pol imp = {s = pol.s ++ imp.s ! pol.p ! AgPol} ;

    UttIP ip = {s = ip.s ! NPCase Nom} ;
    UttIAdv iadv = iadv ;
    UttNP np = {s = addNegation np.isNeg ++ np.s ! NPSep} ;
    UttVP vp = {s = addNegation vp.vptyp.isNeg ++ infVP SCNom Pos (agrP3 Sg) vp Inf1} ;
    UttAdv adv = adv ;
    UttCN cn = {s = cnRef cn} ;
    UttAP np = {s = np.s ! P.False ! NCase Sg Nom} ;
    UttCard n = {s = n.s ! Sg ! Nom} ;
    UttInterj i = i ;

    NoPConj = {s = []} ;
    PConjConj conj = {s = conj.s2} ;

    NoVoc = {s = []} ;
    VocNP np = {s = P.SOFT_BIND ++ "," ++ np.s ! NPSep} ;

oper
  addNegation : P.Bool -> Str = \isNeg -> case isNeg of {P.True => "ei" ; _ => []} ;
}
