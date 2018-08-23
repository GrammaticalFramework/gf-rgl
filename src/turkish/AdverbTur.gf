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

    -- TODO: in order for this to be properly implemented, the subordinating
    -- suffix "-DIK" must be implemented and then the sentence must be
    -- inflected with that. For example, the example sentence "more warmly than
    -- he runs" would be equivalent to "more warmly than his running" where
    -- this "running" gerund is obtained with -DIK. This linearizations must be
    -- revisited once that is done.
    ComparAdvAdjS cadv a s = {
      s = s.s ++ cadv.s ++ a.s ! Sg ! Nom
    } ;

    SubjS = cc2 ;
}
