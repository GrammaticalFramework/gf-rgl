concrete SentenceHye of Sentence = CatHye ** open Prelude,ResHye in {
  lin PredVP np vp = {s = np.s ! Nom ++ vp.s;
                      conditional = \\a,n => np.s ! Nom ++ vp.conditional ! a ! P3 ! n;
                      converb = {imperfective = np.s ! Nom ++ vp.converb.imperfective;
                                 futCon1 = np.s ! Nom ++ vp.converb.futCon1;
                                 futCon2 = np.s ! Nom ++ vp.converb.futCon2;
                                 negative = np.s ! Nom ++ vp.converb.negative;
                                 perfective = np.s ! Nom ++ vp.converb.perfective;
                                 simultaneous = np.s ! Nom ++ vp.converb.simultaneous};
                      passive = np.s ! Nom ++ vp.passive;
                      past = \\_,n => np.s ! Nom ++ vp.past ! P3 ! n;
                      participle = \\p => np.s ! Nom ++ vp.participle ! p;
                      subjunctive = \\a,n => np.s ! Nom ++ vp.subjunctive ! a ! P3 ! n} ;
}
