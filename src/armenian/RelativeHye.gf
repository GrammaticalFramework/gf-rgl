concrete RelativeHye of Relative = CatHye ** open Prelude, ResHye in {
  oper
    relClForm : Cl -> Tense -> Anteriority -> Polarity -> Str = \cl,t,a,p ->
      case a of {
        Anter => cl.anterior ! t ! p;
        Simul => case p of {
          Neg => cl.negative ! t;
          Pos => case t of {
            Pres => cl.s;
            Past => cl.past ! P3 ! Sg;
            Fut => cl.conditional ! Non_Past ! Sg;
            Cond => cl.conditional ! Perfect ! Sg
          }
        }
      } ;
    relVPForm : VP -> Tense -> Anteriority -> Polarity -> Str = \vp,t,a,p ->
      case a of {
        Anter => case <t,p> of {
          <Pres,Pos> => vp.converb.perfective ++ presentAux P3 Sg;
          <Pres,Neg> => negativePresentAux P3 Sg ++ vp.converb.perfective;
          <Past,Pos> => vp.converb.perfective ++ pastAux P3 Sg;
          <Past,Neg> => negativePastAux P3 Sg ++ vp.converb.perfective;
          <Fut,Pos> => vp.converb.perfective ++ copulaVerb.conditional ! Non_Past ! P3 ! Sg;
          <Fut,Neg> => negativePresentAux P3 Sg ++ vp.converb.perfective ++ copulaVerb.converb.futCon1;
          <Cond,Pos> => vp.converb.perfective ++ copulaVerb.conditional ! Perfect ! P3 ! Sg;
          <Cond,Neg> => negativePastAux P3 Sg ++ vp.converb.perfective
        };
        Simul => case <t,p> of {
          <Pres,Pos> => vp.converb.imperfective ++ presentAux P3 Sg;
          <Pres,Neg> => negativePresentAux P3 Sg ++ vp.converb.imperfective;
          <Past,Pos> => vp.past ! P3 ! Sg;
          <Past,Neg> => "չ" ++ BIND ++ vp.past ! P3 ! Sg;
          <Fut,Pos> => vp.conditional ! Non_Past ! P3 ! Sg;
          <Fut,Neg> => negativePresentAux P3 Sg ++ vp.converb.futCon1;
          <Cond,Pos> => vp.conditional ! Perfect ! P3 ! Sg;
          <Cond,Neg> => negativePastAux P3 Sg ++ vp.subjunctive ! Non_Past ! P3 ! Sg
        }
      } ;
  lin
    RelCl cl = {s = \\t,a,p => "որը" ++ relClForm cl t a p} ;
    RelVP rp vp = {s = \\t,a,p => rp.s ++ relVPForm vp t a p} ;
    RelSlash rp slash = {s = \\_,_,_ => rp.s ++ slash.s} ;
    IdRP = {s = "որը"} ;
    FunRP prep np rp = {s = case prep.isPre of {
      True=>prep.s++np.s!prep.c++rp.s;
      False=>np.s!prep.c++prep.s++rp.s
    }} ;
}
