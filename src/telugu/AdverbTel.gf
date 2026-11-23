concrete AdverbTel of Adverb = CatTel ** open ResTel, Prelude in {
  lin
    PositAdvAdj a = {s = a.s} ;
    PositAdAAdj a = {s = a.s} ;
    PrepNP prep np = {s = np.s ! NPC prep.c ++ prep.s} ;
    AdAdv ada adv = {s = ada.s ++ adv.s} ;
    SubjS subj sent = {s = sent.s ++ subj.s} ;
}
