concrete AdverbBel of Adverb = CatBel ** open ResBel in {

lin
  PositAdvAdj a = {s = a.adv} ;
  PrepNP prep np = {s = prepNP prep np} ;
  ComparAdvAdj cadv a np = {s = cadv.s ++ a.adv ++ cadv.p ++ np.s ! Nom} ;
  ComparAdvAdjS cadv a s = {s = cadv.s ++ a.adv ++ cadv.p ++ s.s} ;
  AdAdv ada adv = {s = ada.s ++ adv.s} ;
  PositAdAAdj a = {s = a.adv} ;
  SubjS subj s = {s = subj.s ++ s.s} ;
  AdnCAdv cadv = {s = cadv.s} ;

}
