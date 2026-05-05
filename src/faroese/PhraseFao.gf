concrete PhraseFao of Phrase = CatFao ** open ResFao in {
lin
  PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

  UttS s = s ;
  UttNP np = {s = np.s ! Nom} ;
  UttVP vp = {s = vp.Nonfinite} ;
  UttCN cn = {s = cn.s ! Indef ! Sg ! Nom} ;
  UttAP ap = {s = ap.s ! Masc ! Sg ! Nom} ;
  UttAdv adv = adv ;
  UttInterj i = i ;

  NoPConj = {s = []} ;

  NoVoc = {s = []} ;
}
