concrete AdverbKaz of Adverb = CatKaz ** open ResKaz in {
  lin
    ComparAdvAdj cadv a np = {s=cadv.s ++ a.s ++ cadv.p ++ np.s ! Ablat} ;
    ComparAdvAdjS cadv a s = {s=cadv.s ++ a.s ++ cadv.p ++ s.s} ;
    PositAdAAdj a = {s=a.s} ;
    PositAdvAdj a = {s=a.s} ;
    PrepNP prep np = {s=np.s ! prep.c ++ prep.s} ;
    AdAdv a b = {s=a.s ++ b.s} ;
    SubjS sub s = {s=s.s ++ sub.s} ;
    AdnCAdv c = {s=c.s} ;
}
