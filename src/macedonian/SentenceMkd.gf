concrete SentenceMkd of Sentence = CatMkd ** open Prelude,ResMkd in {

  lin PredVP np vp =
        let n = case np.g of {
                  GSg _ => Sg ;
                  GPl   => Pl
                }
        in {present = \\a => np.s ! RSubj ++ vp.present ! a ! n ! np.p ;
            aorist = np.s ! RSubj ++ vp.aorist ! n ! np.p ;
            participle = {aorist = \\a => np.s ! RSubj ++ vp.participle.aorist ! a ! np.g;
                          perfect = \\a => np.s ! RSubj ++ vp.participle.perfect ! a}} ;

}
