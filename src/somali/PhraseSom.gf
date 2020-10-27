concrete PhraseSom of Phrase = CatSom ** open Prelude, ResSom in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = {s = s.s ! False} ;
    UttQS qs = qs ;
    UttIAdv iadv = iadv ;

    UttImpSg pol imp =
      let ha = case pol.p of {Pos => [] ; Neg => "ha"}
      in {s = ha ++ imp.s ! Sg ! pol.p} ;
    UttImpPl pol imp =
      let ha = case pol.p of {Pos => [] ; Neg => "ha"}
       in {s = ha ++ imp.s ! Pl ! pol.p} ;
    UttImpPol = UttImpSg ;

    UttIP ip = {s = ip.s ! Abs} ;
    UttNP np = {s = np.s ! Abs} ;
    UttVP vp = {s = infVP vp} ;
    UttAdv adv = {s = linAdv adv} ;
    UttCN n = {s = linCN n} ;
    UttCard n = {s = n.s ! Mid} ;
    UttAP ap = { s = ap.s ! AF Sg Abs } ;
    UttInterj i = i ;

    NoPConj = {s = []} ;
    PConjConj conj = {s = conj.s1 ++ conj.s2 ! Indefinite} ;

    NoVoc = {s = []} ;
    VocNP np = { s = "," ++ np.s ! Abs } ; --TODO: vocative exists

}
