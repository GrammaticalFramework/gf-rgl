concrete VerbMkd of Verb = CatMkd ** open Prelude,ResMkd in {

  lin ComplSlash vps np = {present = \\a,n,p => vps.present ! a ! n
                                                  ! p
                                                  ++ np.s ! RObj Acc ;
                           aorist = \\n,p => vps.aorist ! n ! p ++ np.s ! RObj Acc;
                           imperfect = \\a,n,p => vps.imperfect ! a ! n ! p ++ np.s ! RObj Acc;
                           imperative = \\a,n => vps.imperative ! a ! n ++ np.s ! RObj Acc;
                           participle = {aorist = \\a,gn => vps.participle.aorist ! a ! gn ++ np.s ! RObj Acc;
                                         perfect = \\a => vps.participle.perfect ! a ++ np.s ! RObj Acc}} ;
  lin SlashV2a v = v ;

}
