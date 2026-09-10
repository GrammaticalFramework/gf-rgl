concrete QuestionBel of Question = CatBel ** open ResBel, (R = ParamX) in {

lincat
  QVP = {s : Str} ;

lin
  QuestCl cl = cl ;
  QuestVP ip vp = {
    s = \\t,p => ip.s ! Nom ++ vp.s ! t ! p ! ip.a
  } ;
  QuestSlash ip cl = {
    s = \\t,p => cl.c.s ++ ip.s ! cl.c.c ++ cl.s ! t ! p
  } ;
  QuestIAdv iadv cl = {
    s = \\t,p => iadv.s ++ cl.s ! t ! p
  } ;
  QuestIComp icomp np = {
    s = \\t,p => icomp.s ++ np.s ! Nom
  } ;

  IdetCN idet cn = {
    s = \\c => idet.s ! c ! cn.g ++ cn.s ! c ! idet.n ;
    a = {g=cn.g; n=idet.n; p=P3}
  } ;
  IdetIP idet = {
    s = \\c => idet.s ! c ! Masc ;
    a = {g=Masc; n=idet.n; p=P3}
  } ;
  AdvIP ip adv = {
    s = \\c => ip.s ! c ++ adv.s ;
    a = ip.a
  } ;
  IdetQuant iquant num = {
    s = \\c,g => iquant.s ! c ! g ! num.n ++ num.s ! c ! g ;
    n = num.n
  } ;

  PrepIP prep ip = {s = prepNP prep ip} ;
  AdvIAdv iadv adv = {s = iadv.s ++ adv.s} ;
  CompIAdv iadv = iadv ;
  CompIP ip = {s = ip.s ! Nom} ;

  ComplSlashIP vp ip = {s = vp.s ! R.Pres ! R.Pos ! defaultAgr ++ vp.c.s ++ ip.s ! vp.c.c ++ vp.post} ;
  AdvQVP vp iadv = {s = vp.s ! R.Pres ! R.Pos ! defaultAgr ++ iadv.s} ;
  AddAdvQVP qvp iadv = {s = qvp.s ++ iadv.s} ;
  QuestQVP ip qvp = {
    s = \\_,_ => ip.s ! Nom ++ qvp.s
  } ;

}
