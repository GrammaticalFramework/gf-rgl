concrete QuestionFao of Question = CatFao ** open ResFao in {

lincat QVP = {s : Tense => Polarity => Str} ;

lin
  QuestCl cl = {s = cl.Indicative} ;
  QuestVP ip vp = {
    s = \\t,pol => ip.s ++ vp.Indicative ! t ! pol ! Masc ! persNum ip.n P3
  } ;
  QuestSlash ip cls = {
    s = \\t,pol => ip.s ++ cls.s ! t ! pol
  } ;
  QuestIAdv iadv cl = {
    s = \\t,pol => iadv.s ++ cl.Indicative ! t ! pol
  } ;
  QuestIComp icomp np = {
    s = \\t,pol => icomp.s ++ copula ! t ! persNum np.n np.p ++ negStr pol ++ np.s ! Nom
  } ;

  IdetCN idet cn = {
    s = idet.s ++ cn.s ! Indef ! idet.n ! Nom ;
    n = idet.n
  } ;
  IdetIP idet = {
    s = idet.s ;
    n = idet.n
  } ;
  AdvIP ip adv = ip ** {s = ip.s ++ adv.s} ;
  IdetQuant iquant num = {
    s = iquant.s ++ num.s ! Masc ! Nom ;
    n = num.n
  } ;
  PrepIP prep ip = {s = prep.s ++ ip.s} ;
  AdvIAdv iadv adv = {s = iadv.s ++ adv.s} ;
  CompIAdv iadv = iadv ;
  CompIP ip = {s = ip.s} ;

  ComplSlashIP vps ip = {
    s = \\t,pol => vps.Indicative ! t ! PSg P3 ++ vps.particle ++ negStr pol ++ vps.c2.s ++ ip.s ++ vps.sc
  } ;
  AdvQVP vp iadv = {
    s = \\t,pol => vp.Indicative ! t ! pol ! Masc ! PSg P3 ++ iadv.s
  } ;
  AddAdvQVP qvp iadv = {
    s = \\t,pol => qvp.s ! t ! pol ++ iadv.s
  } ;
  QuestQVP ip qvp = {
    s = \\t,pol => ip.s ++ qvp.s ! t ! pol
  } ;
}
