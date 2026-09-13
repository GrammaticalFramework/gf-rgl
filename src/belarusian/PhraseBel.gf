concrete PhraseBel of Phrase = CatBel ** open ResBel in {
lin
  PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

  UttS s = s ;
  UttQS qs = qs ;
  UttImpSg pol imp = {s = pol.s ++ imp.s ! pol.p ! Sg} ;
  UttImpPl pol imp = {s = pol.s ++ imp.s ! pol.p ! Pl} ;
  UttImpPol pol imp = {s = pol.s ++ imp.s ! pol.p ! Pl} ;
  UttIP ip = {s = ip.s ! Nom} ;
  UttIAdv iadv = iadv ;
  UttNP np = {s = np.s ! Nom} ;
  UttAdv adv = adv ;
  UttVP vp = {s = vp.inf} ;
  UttCN cn = {s = cn.s ! Nom ! Sg} ;
  UttCard card = {s = card.s} ;
  UttAP ap = {s = ap.s ! Nom ! GSg Masc} ;
  UttInterj i = i ;

  NoPConj = {s = []} ;
  PConjConj conj = {s = conj.s} ;

  NoVoc = {s = []} ;
  VocNP np = {s = np.s ! Nom} ;
}
