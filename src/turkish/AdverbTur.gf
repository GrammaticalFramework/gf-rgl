concrete AdverbTur of Adverb = CatTur ** open ResTur, Prelude in {
  lin
    PrepNP prep np = {s = np.s ! prep.c ++ prep.s} ;

    always_AdV = {s = "her zaman"} ;
    
    AdAdv = cc2 ;

    -- TODO: test this later; depends on less_CAdv.
    AdnCAdv cadv = { s = cadv.s; c = cadv.c } ;

    ComparAdvAdj cadv a np = {
      s = np.s ! cadv.c ++ cadv.s ++ a.s ! Sg ! cadv.c
    } ;

    -- TODO: inflect the subject to genitive.
    ComparAdvAdjS cadv a s = {
      s = s.s ! SubordSuffixDik ++ cadv.s ++ a.s ! Sg ! Nom
    } ;

    SubjS s1 s2 = {s = s1.s ++ s2.s ! SubordSuffixDik} ;
}
