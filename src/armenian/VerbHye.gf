concrete VerbHye of Verb = CatHye ** open Prelude,ResHye in {
  lin AdvVP vp adv = {s = adv.s ++ vp.s;
                      conditional = \\a,p,n => adv.s ++ vp.conditional ! a ! p ! n;
                      converb = {imperfective = adv.s ++ vp.converb.imperfective;
                                 futCon1 = adv.s ++ vp.converb.futCon1;
                                 futCon2 = adv.s ++ vp.converb.futCon2;
                                 negative = adv.s ++ vp.converb.negative;
                                 perfective = adv.s ++ vp.converb.perfective;
                                 simultaneous = adv.s ++ vp.converb.simultaneous};
                      imperative = \\n => vp.imperative ! n ++ adv.s;
                      passive = adv.s ++ vp.passive;
                      past = \\p,n => adv.s ++ vp.past ! p ! n;
                      participle = \\p => adv.s ++ vp.participle ! p;
                      subjunctive = \\a,p,n => adv.s ++ vp.subjunctive ! a ! p ! n} ;
  lin ComplSlash vp np = {s = vp.s ++ vp.c2.s ++ np.s ! vp.c2.c;
                          conditional = \\a,p,n => vp.conditional ! a ! p ! n
                                                     ++ vp.c2.s ++ np.s ! vp.c2.c;
                          converb = {imperfective = vp.converb.imperfective
                                                      ++ vp.c2.s ++ np.s ! vp.c2.c;
                                     futCon1 = vp.converb.futCon1 ++ vp.c2.s ++ np.s ! vp.c2.c;
                                     futCon2 = vp.converb.futCon2 ++ vp.c2.s ++ np.s ! vp.c2.c;
                                     negative = vp.converb.negative ++ vp.c2.s ++ np.s ! vp.c2.c;
                                     perfective = vp.converb.perfective
                                                    ++ vp.c2.s ++ np.s ! vp.c2.c;
                                     simultaneous = vp.converb.simultaneous
                                                      ++ vp.c2.s ++ np.s ! vp.c2.c};
                          imperative = \\n => vp.imperative ! n ++ vp.c2.s ++ np.s ! Nom;
                          passive = vp.passive ++ vp.c2.s ++ np.s ! vp.c2.c;
                          past = \\p,n => vp.past ! p ! n ++ vp.c2.s ++ np.s ! vp.c2.c;
                          participle = \\p => vp.participle ! p ++ vp.c2.s ++ np.s ! vp.c2.c;
                          subjunctive = \\a,p,n => vp.subjunctive ! a ! p ! n
                                                     ++ vp.c2.s ++ np.s ! vp.c2.c} ;
  lin SlashV2a v = v ;
  lin UseV v = v ;
}
