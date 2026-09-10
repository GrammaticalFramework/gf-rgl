concrete AdverbHye of Adverb = CatHye ** open Prelude,ResHye in {
  lin PrepNP p np = {s = case p.isPre of {
                           False => np.s ! p.c ++ p.s;
                           True  => p.s ++ np.s ! p.c
                         }} ;
}
