concrete QuestionKaz of Question = CatKaz ** open ResKaz, ParadigmsKaz in {
  lin
    QuestCl cl = cl ;
    QuestVP ip vp = mkClause {s=\\_ => ip.s;a=defaultAgr} vp ;
    QuestSlash ip cl = cl ;
    QuestIAdv i cl = {
      pres=\\p => i.s ++ cl.pres ! p;
      past=\\p => i.s ++ cl.past ! p;
      fut=\\p => i.s ++ cl.fut ! p;
      cond=\\p => i.s ++ cl.cond ! p;
      anter=\\p => i.s ++ cl.anter ! p
    } ;
    QuestIComp i np = mkClause np (prefixVerb i.s (mkV "болу")) ;
    IdetCN det cn = {s=det.s ++ cn.s ! Nom ! det.n} ;
    IdetIP det = {s=det.s} ;
    IdetQuant q num = {s=q.s ++ num.s;n=num.n} ;
    AdvIP ip adv = {s=adv.s ++ ip.s} ;
    PrepIP prep ip = {s=ip.s ++ prep.s} ;
    AdvIAdv i adv = {s=adv.s ++ i.s} ;
    CompIP ip = ip ;
}
