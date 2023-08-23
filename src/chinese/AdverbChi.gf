concrete AdverbChi of Adverb = CatChi **
  open ResChi, Prelude in {

  lin
    PositAdvAdj a = {s = a.s!Attr ++ "地" ; advType = ATManner ; hasDe = False} ; ---- for all adjs?

    PrepNP prep np = ss (appPrep prep (linNP np)) ** {advType = prep.advType ; hasDe = prep.hasDe} ; --- should depend on np too ?

    ComparAdvAdj cadv a np = ss (a.s!Attr ++ cadv.s ++ cadv.p ++ (linNP np)) ** {advType = ATManner ; hasDe = False} ;

    ComparAdvAdjS cadv a s = ss (a.s!Attr ++ cadv.s ++ cadv.p ++ linS s) ** {advType = ATManner ; hasDe = False} ;

    AdAdv ad adv = adv ** {s = ad.s ++ adv.s} ;

    SubjS subj s = ss (subj.prePart ++ linS s ++ subj.sufPart)  ** {advType = ATTime ; hasDe = False} ;

    AdnCAdv cadv = ss (cadv.s ++ conjThat) ** {advType = ATManner ; hasDe = False} ; -----

    PositAdAAdj a = {s = a.s!Attr} ; ----

}
