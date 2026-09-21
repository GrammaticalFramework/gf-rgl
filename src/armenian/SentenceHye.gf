concrete SentenceHye of Sentence = CatHye ** open Prelude,ResHye in {
  oper negAux : Agr -> Str = \a -> case <a.p,a.n> of {
    <P1,Sg> => "չեմ"; <P2,Sg> => "չես"; <P3,Sg> => "չի";
    <P1,Pl> => "չենք"; <P2,Pl> => "չեք"; <P3,Pl> => "չեն"
  } ;
  lin PredVP np vp = {s = np.s ! Nom ++ vp.converb.imperfective ++
                          case <np.a.p,np.a.n> of {
                            <P1,Sg> => "եմ"; <P2,Sg> => "ես"; <P3,Sg> => "է";
                            <P1,Pl> => "ենք"; <P2,Pl> => "եք"; <P3,Pl> => "են"
                          };
                      negative = table {
                        Pres => np.s ! Nom ++ negAux np.a ++ vp.converb.imperfective;
                        Past => np.s ! Nom ++ "չ" ++ BIND ++ vp.past ! np.a.p ! np.a.n;
                        Fut => np.s ! Nom ++ negAux np.a ++ vp.converb.futCon1;
                        Cond => np.s ! Nom ++ "չ" ++ BIND ++ vp.conditional ! Non_Past ! np.a.p ! np.a.n
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
  lin PredSCVP sc vp = {s = sc.s ++ vp.s;
    negative=table {_ => sc.s ++ "չ" ++ BIND ++ vp.s};
    conditional=\\a,n => sc.s ++ vp.conditional ! a ! P3 ! n;
    converb={imperfective=sc.s ++ vp.converb.imperfective; futCon1=sc.s ++ vp.converb.futCon1;
      futCon2=sc.s ++ vp.converb.futCon2; negative=sc.s ++ vp.converb.negative;
      perfective=sc.s ++ vp.converb.perfective; simultaneous=sc.s ++ vp.converb.simultaneous};
    passive=sc.s ++ vp.passive; past=\\p,n => sc.s ++ vp.past ! p ! n;
    participle=\\p => sc.s ++ vp.participle ! p;
    subjunctive=\\a,n => sc.s ++ vp.subjunctive ! a ! P3 ! n} ;
  lin UseCl temp pol cl = {s = case pol.p of {
    Neg => cl.negative ! temp.t;
    Pos => case <temp.t,temp.a> of {
      <Pres,Simul> => cl.s;
      <Pres,Anter> => cl.converb.perfective;
      <Past,Simul> => cl.past ! P3 ! Sg;
      <Past,Anter> => cl.converb.perfective;
      <Fut,Simul> => cl.conditional ! Non_Past ! Sg;
      <Fut,Anter> => cl.conditional ! Perfect ! Sg;
      <Cond,Simul> => cl.conditional ! Non_Past ! Sg;
      <Cond,Anter> => cl.conditional ! Perfect ! Sg
    }
  }} ;
  lin UseQCl temp pol qcl = {s = case pol.p of {Pos => qcl.s; Neg => "չ" ++ qcl.s}} ;
  lin UseRCl temp pol rcl = {s = case pol.p of {Pos => rcl.s; Neg => "չ" ++ rcl.s}} ;
  lin UseSlash temp pol slash = {s = case pol.p of {Pos => slash.s; Neg => "չ" ++ slash.s}} ;
  lin AdvS adv s = {s = adv.s ++ s.s} ;
  lin ExtAdvS adv s = {s = adv.s ++ "," ++ s.s} ;
  lin SSubjS s1 subj s2 = {s = s1.s ++ subj.s ++ s2.s} ;
  lin RelS s rs = {s = s.s ++ "," ++ rs.s} ;
  lin ImpVP vp = {s = vp.imperative ! Sg} ;
  lin AdvImp adv imp = {s = adv.s ++ imp.s} ;
  lin EmbedS s = s ;
  lin EmbedQS qs = qs ;
  lin SlashVP np vp = {s = np.s ! Nom ++ vp.s} ;
  lin AdvSlash slash adv = {s = slash.s ++ adv.s} ;
  lin SlashPrep cl prep = {s = cl.s ++ prep.s} ;
  lin SlashVS np vs slash = {s = np.s ! Nom ++ vs.s ++ slash.s} ;
}
