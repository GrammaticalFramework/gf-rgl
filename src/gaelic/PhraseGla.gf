concrete PhraseGla of Phrase = CatGla ** open Prelude, ResGla in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = s ;

    UttQS qs = qs ;
    UttIAdv iadv = iadv ;

    UttNP np = {s = linNP np} ;

    UttIP ip = ip ;
    UttImpSg pol imp = {s = case pol.p of {Pos => imp.s ; Neg => "na" ++ imp.s}} ;
    UttImpPl pol imp = {s = case pol.p of {Pos => imp.s ; Neg => "na" ++ imp.s}} ;
    UttImpPol pol imp = {s = case pol.p of {Pos => imp.s ; Neg => "na" ++ imp.s}} ;
    UttVP vp = {s = vp.s} ;
    UttAP ap = { s = ap.s ! ASg NOM Masc } ;
    UttAdv adv = {s = adv.s} ;
    UttCN n = {s = n.s ! NOM ! Indef ! Sg} ;
    UttCard n = {s = n.s} ;
    UttInterj i = i ;
    NoPConj = {s = []} ;
    PConjConj conj = {s = conj.s1 ++ conj.s2} ;

    NoVoc = {s = []} ;
    VocNP np = {s = "," ++ np.art ! NOM ++ np.voc} ;  --guessed

}
