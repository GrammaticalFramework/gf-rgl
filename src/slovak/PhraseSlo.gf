concrete PhraseSlo of Phrase = CatSlo ** open Prelude, ResSlo in {

lin
    UttS s = s ;
    UttAdv adv = adv ;
    UttCN cn = {s = cn.s ! Sg ! Nom} ;
    UttAP ap = {s = ap.s ! Masc Anim ! Sg ! Nom} ;
    UttNP np = {s = np.s ! Nom} ;

    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;


    NoPConj = {s = []} ;
    PConjConj conj = {s = conj.s2} ;

    NoVoc = {s = []} ;
    VocNP np = {s = np.s ! Nom} ; 

}
