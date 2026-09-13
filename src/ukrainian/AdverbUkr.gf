concrete AdverbUkr of Adverb = CatUkr ** open ResUkr, (R = ParamX) in {

lin
  PositAdvAdj a = {s = a.s ! Nom ! GSg Neuter} ;
  PrepNP prep np = {s = prepNP prep np} ;

  ComparAdvAdj cadv a np = {
    s = cadv.s ++ a.s ! Nom ! GSg Neuter ++ cadv.p ++ np.s ! Nom
  } ;
  ComparAdvAdjS cadv a s = {
    s = cadv.s ++ a.s ! Nom ! GSg Neuter ++ cadv.p ++ s.s
  } ;
  AdAdv ada adv = {s = ada.s ++ adv.s} ;
  PositAdAAdj a = {s = a.s ! Nom ! GSg Neuter} ;
  SubjS subj s = {s = subj.s ++ s.s} ;
  AdnCAdv cadv = {s = cadv.s ++ cadv.p} ;
}
