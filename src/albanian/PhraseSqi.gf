concrete PhraseSqi of Phrase = CatSqi ** open Prelude, ResSqi in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttNP np = {s = np.s ! Nom} ;
    UttInterj i = i ;

    NoPConj = {s = []} ;

    NoVoc = {s = []} ;

}
