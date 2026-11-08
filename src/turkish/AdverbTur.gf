concrete AdverbTur of Adverb = CatTur ** open ResTur, Prelude in {
  lin
    PrepNP prep np = {s = np.s ! prep.c ++ prep.s} ;
    
    AdAdv = cc2 ;

    -- TODO: test this later; depends on less_CAdv.
    AdnCAdv cadv = { s = cadv.s; c = cadv.c } ;

    ComparAdvAdj cadv a np = {
      s = np.s ! cadv.c ++ cadv.s ++ a.s ! Sg ! cadv.c
    } ;

    -- TODO: inflect the subject to genitive.
    ComparAdvAdjS cadv a s = {
      s = s.s ++ cadv.s ++ a.s ! Sg ! Nom
    } ;

    SubjS subj s = {s = subj.s ++ s.s} ;

    PositAdvAdj a = {s = a.adv} ;
    PositAdAAdj a = {s = a.adv} ;

}
