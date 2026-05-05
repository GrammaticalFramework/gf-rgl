concrete PhraseFao of Phrase = CatFao ** open ResFao in {
lin
  PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

  UttS s = s ;
  UttNP np = {s = np.s ! Nom} ;
  UttVP vp = {s = vp.Nonfinite} ;
  UttInterj i = i ;

  NoPConj = {s = []} ;

  NoVoc = {s = []} ;
}
