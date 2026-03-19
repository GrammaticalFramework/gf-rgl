concrete AdverbMkd of Adverb = CatMkd ** open Prelude,ResMkd in {
  flags
    coding = "UTF-8" ;
  lin PrepNP p np = {s = p.s ++ np.s} ;
}