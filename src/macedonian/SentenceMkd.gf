concrete SentenceMkd of Sentence = CatMkd ** open Prelude,ResMkd in {
  flags
    coding = "UTF-8" ;
  lin PredVP np vp = {present = \\a => np.s ++ vp.present ! a ! np.n ! np.p ;
                      aorist = np.s ++ vp.aorist ! np.n ! np.p ;
                      participle = {aorist = \\a => np.s ++ vp.participle.aorist ! a ! 
                                                             case np.n of {
                                                               Sg => GSg np.g;
                                                               Pl => GPl
                                                             };
                                    perfect = \\a => np.s ++ vp.participle.perfect ! a}} ;
}
