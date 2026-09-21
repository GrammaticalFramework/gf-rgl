concrete QuestionHye of Question = CatHye ** open ResHye in {
  lincat QVP = {s : Str} ;
  lin
    QuestCl cl = {s = cl.s ++ "՞"} ;
    QuestVP ip vp = {s = ip.s ++ vp.s ++ "՞"} ;
    QuestSlash ip slash = {s = ip.s ++ slash.s ++ "՞"} ;
    QuestIAdv adv cl = {s = adv.s ++ cl.s ++ "՞"} ;
    QuestIComp comp np = {s = comp.s ++ np.s ! Nom ++ "՞"} ;
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
    QuestQVP ip qvp = {s = ip.s ++ qvp.s ++ "՞"} ;
}
