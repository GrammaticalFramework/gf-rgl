concrete PhraseSqi of Phrase = CatSqi ** open Prelude, ResSqi in {

  lin
    PhrUtt pconj utt voc = {s = pconj.s ++ utt.s ++ voc.s} ;

    UttS s = s ;
    UttQS s = s ;
    UttImpSg p i = {s=i.s ! p.p ! Sg} ;
    UttImpPl p i = {s=i.s ! p.p ! Pl} ;
    UttImpPol p i = {s=i.s ! p.p ! Pl} ;
    UttIP ip = {s=ip.s} ;
    UttIAdv a = a ;
    UttNP np = {s = np.s ! Nom} ;
    UttAdv a = a ;
    UttVP vp = {s="të" ++ vp.subjunctive ! Sg ! P3 ! Masc ! Nom} ;
    UttCN cn = {s=cn.s ! Indef ! Nom ! Sg} ;
    UttCard c = c ;
    UttAP ap = {s=ap.s ! Indef ! Nom ! Masc ! Sg} ;
    UttInterj i = i ;

    NoPConj = {s = []} ;
    PConjConj c = c ;

    NoVoc = {s = []} ;
    VocNP np = {s=SOFT_BIND ++ "," ++ np.s ! Nom} ;

}
