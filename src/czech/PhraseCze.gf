concrete PhraseCze of Phrase = CatCze ** open Prelude, ResCze in {

lin
    UttS s = s ;
    UttAdv adv = adv ;
    UttCN cn = {s = cn.s ! Sg ! Nom} ;
    UttAP ap = {s = ap.s ! Masc Anim ! Sg ! Nom} ;
    UttNP np = {s = np.s ! Nom} ;
    UttVP vp = let agr = Ag Neutr Sg P3 in {s = vp.clit ! agr ++ vp.verb.inf ++ vp.compl ! agr} ; 

    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;


    NoPConj = {s = []} ;
    PConjConj conj = {s = conj.s2} ;

    NoVoc = {s = []} ;
    VocNP np = {s = np.s ! Voc} ; 

}
