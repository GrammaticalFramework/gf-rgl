concrete AdverbSqi of Adverb = CatSqi ** open Prelude, ResSqi in {
lin
  PositAdvAdj a = {s=a.s ! Nom ! Masc ! Sg} ;
  PrepNP p np = {s=p.s ++ np.s ! p.c} ;
  ComparAdvAdj c a np = {s=c.s ++ a.s ! Nom ! Masc ! Sg ++ c.p ++ np.s ! Ablat} ;
  ComparAdvAdjS c a s = {s=c.s ++ a.s ! Nom ! Masc ! Sg ++ c.p ++ s.s} ;
  AdAdv a adv = {s=a.s ++ adv.s} ;
  PositAdAAdj a = {s=a.s ! Nom ! Masc ! Sg} ;
  SubjS subj s = {s=subj.s ++ s.s} ;
  AdnCAdv c = {s=c.s ++ c.p} ;
}
