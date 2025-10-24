concrete AdverbMkd of Adverb = CatMkd ** open Prelude,ResMkd in {
  lin AdAdv a a2 = {s = a.s ++ a2.s} ;
  lin AdnCAdv c = {s = c.s} ;
  lin ComparAdvAdj c a np = {s = c.s
                                   ++ a.s ! Indef ! np.a.g ++ np.s ! RSubj} ;
  lin ComparAdvAdjS c a s = {s = c.s
                                   ++ a.s ! Indef ! GSg Masc ++ s.s} ;
  lin PositAdAAdj a = {s = a.s ! Indef ! GSg Masc} ;
  lin PositAdvAdj a = {s = a.s ! Indef ! GSg Masc} ;
  lin PrepNP p np = {s = p.s ++ np.s ! RPrep} ;
  lin SubjS s s2 = {s = s.s ++ s2.s} ;
}
