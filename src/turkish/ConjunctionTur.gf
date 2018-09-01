concrete ConjunctionTur of Conjunction =
  CatTur ** open ResTur, Coordination, Prelude in {

  lin

    -- TODO: ap2.s seems to irrelevant; investigate why.
    BaseAP ap1 ap2 = {
      s = ap1.s ! Sg ! Nom
    } ;

}
