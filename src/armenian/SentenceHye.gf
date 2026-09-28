concrete SentenceHye of Sentence = CatHye ** open Prelude,ResHye in {
  lin PredVP np vp = {s = np.s ! Nom ++ vp.converb.imperfective ++
                          presentAux np.a.p np.a.n;
                      negative = table {
                        Pres => np.s ! Nom ++ negativePresentAux np.a.p np.a.n ++ vp.converb.imperfective;
                        Past => np.s ! Nom ++ "չ" ++ BIND ++ vp.past ! np.a.p ! np.a.n;
                        Fut => np.s ! Nom ++ negativePresentAux np.a.p np.a.n ++ vp.converb.futCon1;
                        Cond => np.s ! Nom ++ "չ" ++ BIND ++ vp.conditional ! Non_Past ! np.a.p ! np.a.n
                      };
                      anterior = table {
                        Pres => table {
                          Pos => np.s ! Nom ++ vp.converb.perfective ++ presentAux np.a.p np.a.n;
                          Neg => np.s ! Nom ++ negativePresentAux np.a.p np.a.n ++ vp.converb.perfective
                        };
                        Past => table {
                          Pos => np.s ! Nom ++ vp.converb.perfective ++ pastAux np.a.p np.a.n;
                          Neg => np.s ! Nom ++ negativePastAux np.a.p np.a.n ++ vp.converb.perfective
                        };
                        Fut => table {
                          Pos => np.s ! Nom ++ vp.converb.perfective ++ copulaVerb.conditional ! Non_Past ! np.a.p ! np.a.n;
                          Neg => np.s ! Nom ++ negativePresentAux np.a.p np.a.n ++ vp.converb.perfective ++ copulaVerb.converb.futCon1
                        };
                        Cond => table {
                          Pos => np.s ! Nom ++ vp.converb.perfective ++ copulaVerb.conditional ! Perfect ! np.a.p ! np.a.n;
                          Neg => np.s ! Nom ++ negativePastAux np.a.p np.a.n ++ vp.converb.perfective
                        }
                      };
                      conditional = \\a,_ => np.s ! Nom ++ vp.conditional ! a ! np.a.p ! np.a.n;
                      converb = {imperfective = np.s ! Nom ++ vp.converb.imperfective;
                                 futCon1 = np.s ! Nom ++ vp.converb.futCon1;
                                 futCon2 = np.s ! Nom ++ vp.converb.futCon2;
                                 negative = np.s ! Nom ++ vp.converb.negative;
                                 perfective = np.s ! Nom ++ vp.converb.perfective;
                                 simultaneous = np.s ! Nom ++ vp.converb.simultaneous};
                      passive = np.s ! Nom ++ vp.passive;
                      past = \\_,_ => np.s ! Nom ++ vp.past ! np.a.p ! np.a.n;
                      participle = \\p => np.s ! Nom ++ vp.participle ! p;
                      subjunctive = \\a,_ => np.s ! Nom ++ vp.subjunctive ! a ! np.a.p ! np.a.n} ;
  lin PredSCVP sc vp = {s = sc.s ++ vp.converb.imperfective ++ presentAux P3 Sg;
    negative=table {
      Pres => sc.s ++ negativePresentAux P3 Sg ++ vp.converb.imperfective;
      Past => sc.s ++ "չ" ++ BIND ++ vp.past ! P3 ! Sg;
      Fut => sc.s ++ negativePresentAux P3 Sg ++ vp.converb.futCon1;
      Cond => sc.s ++ "չ" ++ BIND ++ vp.conditional ! Non_Past ! P3 ! Sg
    };
    anterior=table {
      Pres=>table {Pos=>sc.s++vp.converb.perfective++presentAux P3 Sg;
                   Neg=>sc.s++negativePresentAux P3 Sg++vp.converb.perfective};
      Past=>table {Pos=>sc.s++vp.converb.perfective++pastAux P3 Sg;
                   Neg=>sc.s++negativePastAux P3 Sg++vp.converb.perfective};
      Fut=>table {Pos=>sc.s++vp.converb.perfective++copulaVerb.conditional!Non_Past!P3!Sg;
                  Neg=>sc.s++negativePresentAux P3 Sg++vp.converb.perfective++copulaVerb.converb.futCon1};
      Cond=>table {Pos=>sc.s++vp.converb.perfective++copulaVerb.conditional!Perfect!P3!Sg;
                   Neg=>sc.s++negativePastAux P3 Sg++vp.converb.perfective}
    };
    conditional=\\a,_ => sc.s ++ vp.conditional ! a ! P3 ! Sg;
    converb={imperfective=sc.s ++ vp.converb.imperfective; futCon1=sc.s ++ vp.converb.futCon1;
      futCon2=sc.s ++ vp.converb.futCon2; negative=sc.s ++ vp.converb.negative;
      perfective=sc.s ++ vp.converb.perfective; simultaneous=sc.s ++ vp.converb.simultaneous};
    passive=sc.s ++ vp.passive; past=\\_,_ => sc.s ++ vp.past ! P3 ! Sg;
    participle=\\p => sc.s ++ vp.participle ! p;
    subjunctive=\\a,_ => sc.s ++ vp.subjunctive ! a ! P3 ! Sg} ;
  lin UseCl temp pol cl = {s = case temp.a of {
    Anter => cl.anterior ! temp.t ! pol.p;
    Simul => case pol.p of {
      Neg => cl.negative ! temp.t;
      Pos => case temp.t of {
        Pres => cl.s;
        Past => cl.past ! P3 ! Sg;
        Fut => cl.conditional ! Non_Past ! Sg;
        Cond => cl.conditional ! Perfect ! Sg
      }
    }
  }} ;
  lin UseQCl temp pol qcl = {s = qcl.s ! temp.t ! temp.a ! pol.p} ;
  lin UseRCl temp pol rcl = {s = case pol.p of {Pos => rcl.s; Neg => "չ" ++ rcl.s}} ;
  lin UseSlash temp pol slash = {s = case pol.p of {Pos => slash.s; Neg => "չ" ++ slash.s}} ;
  lin AdvS adv s = {s = adv.s ++ s.s} ;
  lin ExtAdvS adv s = {s = adv.s ++ "," ++ s.s} ;
  lin SSubjS s1 subj s2 = {s = s1.s ++ subj.s ++ s2.s} ;
  lin RelS s rs = {s = s.s ++ "," ++ rs.s} ;
  lin ImpVP vp = {s = vp.imperative} ;
  lin AdvImp adv imp = {s = \\n => adv.s ++ imp.s ! n} ;
  lin EmbedS s = s ;
  lin EmbedQS qs = qs ;
  lin SlashVP np vp = {s = np.s ! Nom ++ vp.s} ;
  lin AdvSlash slash adv = {s = slash.s ++ adv.s} ;
  lin SlashPrep cl prep = {s = cl.s ++ prep.s} ;
  lin SlashVS np vs slash = {s = np.s ! Nom ++ vs.s ++ slash.s} ;
}
