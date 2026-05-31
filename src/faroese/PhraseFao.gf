concrete PhraseFao of Phrase = CatFao ** open ResFao in {
lin
  PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

  UttS s = s ;
  UttQS qs = qs ;
  UttImpSg pol imp = {s = imp.s ! pol.p ! Sg} ;
  UttImpPl pol imp = {s = imp.s ! pol.p ! Pl} ;
  UttImpPol pol imp = {s = imp.s ! pol.p ! Pl} ;
  UttIP ip = {s = ip.s} ;
  UttIAdv iadv = iadv ;
  UttNP np = {s = np.s ! Nom} ;
  UttVP vp = {s = vp.Nonfinite} ;
  UttCN cn = {s = cn.s ! Indef ! Sg ! Nom} ;
  UttCard card = {s = card.s ! Neuter ! Nom} ;
  UttAP ap = {s = ap.s ! Masc ! Sg ! Nom} ;
  UttAdv adv = adv ;
  UttInterj i = i ;

  NoPConj = {s = []} ;
  PConjConj conj = conj ;

  NoVoc = {s = []} ;
  VocNP np = {s = "," ++ np.s ! Nom} ;
}
