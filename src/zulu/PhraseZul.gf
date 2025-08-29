concrete PhraseZul of Phrase = CatZul ** open Prelude, ParamX, ResZul in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;
  --
    UttS sent = { s = sent.s } ;
    UttQS sent = { s = sent.qword_pre ++ sent.s ++ sent.qword_post } ;
    UttImpSg pol imp = { s = pol.s ++ imp.s!Sg!pol.p } ;
    UttImpPl pol imp = {s = pol.s ++ imp.s!Pl!pol.p } ;
  --   UttImpPol pol imp = {s = pol.s ++ imp.s ! pol.p ! ImpF Sg True} ;

  --   UttIP ip = {s = ip.s ! npNom} ; --- Acc also
  --   UttIAdv iadv = iadv ;
    UttNP np = {s = np.s ! NFull} ;
  --   UttVP vp = {s = infVP VVInf vp False Simul CPos (agrP3 Sg)} ;
  --   UttAdv adv = adv ;
  --   UttCN n = {s = n.s ! Sg ! Nom} ;
  --   UttCard n = {s = n.s ! False ! Nom} ;
    UttAP ap = {s = ap.s ! AF1} ;
    UttInterj i = i ;

    NoPConj = {s = []} ;
  --   PConjConj conj = {s = conj.s2} ; ---

    NoVoc = {s = []} ;
    VocNP np = {s = np.s ! NReduced } ;

}
