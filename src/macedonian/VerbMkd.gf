concrete VerbMkd of Verb = CatMkd ** open Prelude,ResMkd in {

  lin ComplSlash vps np = {present = \\a,n,p => vps.present ! a ! n
                                                  ! p
                                                  ++ np.s;
                           aorist = \\n,p => vps.aorist ! n ! p ++ np.s;
                           imperfect = \\a,n,p => vps.imperfect ! a ! n ! p ++ np.s;
                           imperative = \\a,n => vps.imperative ! a ! n ++ np.s;
                           participle = {aorist = \\a,gn => vps.participle.aorist ! a ! gn ++ np.s;
                                         perfect = \\a => vps.participle.perfect ! a ++ np.s}} ;
}
