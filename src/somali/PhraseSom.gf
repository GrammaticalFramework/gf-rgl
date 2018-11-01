concrete PhraseSom of Phrase = CatSom ** open Prelude, ResSom in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = s ;
    -- UttQS qs = qs ;

    UttImpSg pol imp =
      let ma = case pol.p of { Pos => [] ; Neg => "ma" }
      in { s = ma ++ imp.s } ;
    UttImpPl = UttImpSg ;
    UttImpPol = UttImpSg ;

    UttIP ip = { s = ip.s ! Abs} ;
    UttIAdv iadv = iadv ;
    UttNP np = { s = np.s ! Abs} ;
    UttVP vp = { s = linVP vp } ;
    UttAdv adv = adv ;
    UttCN n = {s = linCN n } ;
    UttCard n = n ;
    UttAP ap = { s = ap.s ! AF Sg Abs } ;
    UttInterj i = i ;

    NoPConj = {s = []} ;
    PConjConj conj = { s = conj.s1 ++ conj.s2 } ;

    NoVoc = {s = []} ;
    VocNP np = { s = "," ++ np.s ! Abs } ; --TODO: vocative exists

}
