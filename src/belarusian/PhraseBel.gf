concrete PhraseBel of Phrase = CatBel ** {
lin
  PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

  UttS s = s ;
  UttInterj i = i ;

  NoPConj = {s = []} ;

  NoVoc = {s = []} ;
}
