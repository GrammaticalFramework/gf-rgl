concrete PhraseKaz of Phrase = CatKaz ** open Prelude, ResKaz in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = s ;
    UttInterj i = i ;

    NoPConj = {s = []} ;

    NoVoc = {s = []} ;

}
