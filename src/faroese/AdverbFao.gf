concrete AdverbFao of Adverb = CatFao ** open Prelude,ResFao in {
  lin
    PositAdvAdj a = {s = a.s ! Neuter ! Sg ! Nom} ;
    PrepNP p np = {s = p.s ++ np.s ! p.c} ;
    ComparAdvAdj cadv a np = {s = cadv.s ++ a.s ! Neuter ! Sg ! Nom ++ cadv.p ++ np.s ! Nom} ;
    ComparAdvAdjS cadv a s = {s = cadv.s ++ a.s ! Neuter ! Sg ! Nom ++ cadv.p ++ s.s} ;
    AdAdv ada adv = {s = ada.s ++ adv.s} ;
    PositAdAAdj a = {s = a.s ! Neuter ! Sg ! Nom} ;
    SubjS subj s = {s = subj.s ++ s.s} ;
    AdnCAdv cadv = {s = cadv.s} ;
}
