concrete QuestionHye of Question = CatHye ** open Prelude, ResHye in {
  lincat QVP = {s : Str} ;
  oper
    markFinal : Str -> Str = \s -> s ++ BIND ++ "՞" ;
    clForm : Cl -> Tense -> Anteriority -> Polarity -> Str = \cl,t,a,p ->
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
    vpForm : VP -> Tense -> Anteriority -> Polarity -> Str = \vp,t,a,p ->
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
          <Cond,Neg> => "չ" ++ BIND ++ vp.conditional ! Perfect ! P3 ! Sg
        }
      } ;
  lin
    QuestCl cl = {s = \\t,a,p => markFinal (clForm cl t a p)} ;
    QuestVP ip vp = {s = \\t,a,p => ip.s ++ vpForm vp t a p} ;
    QuestSlash ip slash = {s = \\_,_,_ => ip.s ++ slash.s} ;
    QuestIAdv adv cl = {s = \\t,a,p => adv.s ++ clForm cl t a p} ;
    QuestIComp comp np = {s = \\_,_,_ => comp.s ++ np.s ! Nom} ;
    IdetCN det cn = {s = det.s ++ cn.s ! Indef ! Nom ! Sg} ;
    IdetIP det = {s = det.s} ;
    AdvIP ip adv = {s = ip.s ++ adv.s} ;
    IdetQuant quant num = {s = quant.s ++ num.s} ;
    PrepIP prep ip = {s = case prep.isPre of {True=>prep.s++ip.s;False=>ip.s++prep.s}} ;
    AdvIAdv iadv adv = {s = iadv.s ++ adv.s} ;
    CompIAdv adv = adv ;
    CompIP ip = ip ;
    ComplSlashIP slash ip = {s = slash.s ++ ip.s} ;
    AdvQVP vp adv = {s = vp.s ++ adv.s} ;
    AddAdvQVP qvp adv = {s = qvp.s ++ adv.s} ;
    QuestQVP ip qvp = {s = \\_,_,_ => ip.s ++ qvp.s} ;
}
