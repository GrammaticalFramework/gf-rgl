concrete QuestionTur of Question = CatTur ** open ResTur, Prelude in {

lin
  AdvIAdv iadv adv = {s = adv.s ++ iadv.s} ;
  AdvIP ip adv = {s = adv.s ++ ip.s} ;
  PrepIP prep ip = {s = ip.s ++ prep.s} ;
  CompIP ip = {s = ip.s} ;
  CompIAdv adv = {s = adv.s} ;
  IdetQuant iq num = {s = iq.s ++ num.s ! Sg ! Nom} ;
  IdetCN idet cn = {s = idet.s ++ cn.s ! Sg ! Nom} ;
  IdetIP idet = {s = idet.s} ;

  QuestIComp ic np = {
    s = \\t,a,p => np.s ! Nom ++ ic.s ++ "mi"
  } ;
  QuestIAdv iadv cl = {
    s = \\t,a,p => iadv.s ++ cl.s ! t ! a ! p
  } ;
  QuestSlash ip cl = {
    s = \\t,a,p => ip.s ++ cl.s ! t ! a ! p
  } ;
  QuestVP ip vp = {
    s = \\t,a,p => ip.s ++ vp.compl ++ vp.s ! Perf ! VFin t a p (agrP3 Sg)
  } ;
  QuestCl cl = {
    s = \\t,a,p => cl.s ! t ! a ! p ++ "mi"
  } ;

}
