concrete QuestionUkr of Question = CatUkr ** open ResUkr, (R = ParamX) in {

lincat QVP = {s : R.Tense => R.Polarity => Gender => Number => Person => Str} ;

lin
  QuestCl cl = cl ;
  QuestVP ip vp = {
    s = \\t,pol => ip.s ! Nom ++ vp.s ! t ! pol ! ip.g ! ip.n ! ip.p
  } ;
  QuestSlash ip cls = {
    s = \\t,pol => ip.s ! cls.c.c ++ cls.s ! t ! pol
  } ;
  QuestIAdv iadv cl = {
    s = \\t,pol => iadv.s ++ cl.s ! t ! pol
  } ;
  QuestIComp icomp np = {
    s = \\t,pol => icomp.s ++ np.s ! Nom
  } ;

  IdetCN idet cn = {
    s = \\c => idet.s ! c ! cn.g ++ cn.s ! c ! idet.n ;
    g = cn.g ;
    n = idet.n ;
    p = P3
  } ;
  IdetIP idet = {
    s = \\c => idet.s ! c ! Masc ;
    g = Masc ;
    n = idet.n ;
    p = P3
  } ;
  AdvIP ip adv = ip ** {s = \\c => ip.s ! c ++ adv.s} ;
  IdetQuant iquant num = {
    s = \\c,g => iquant.s ! c ! g ! num.n ++ num.s ;
    n = num.n
  } ;
  PrepIP prep ip = {s = prepNP prep ip} ;
  AdvIAdv iadv adv = {s = iadv.s ++ adv.s} ;
  CompIAdv iadv = iadv ;
  CompIP ip = {s = ip.s ! Nom} ;

  ComplSlashIP slash ip = {
    s = \\t,pol,g,n,p => slash.s ! t ! pol ! g ! n ! p ++ ip.s ! slash.c.c ++ slash.post
  } ;
  AdvQVP vp iadv = {
    s = \\t,pol,g,n,p => vp.s ! t ! pol ! g ! n ! p ++ iadv.s
  } ;
  AddAdvQVP qvp iadv = {
    s = \\t,pol,g,n,p => qvp.s ! t ! pol ! g ! n ! p ++ iadv.s
  } ;
  QuestQVP ip qvp = {
    s = \\t,pol => ip.s ! Nom ++ qvp.s ! t ! pol ! ip.g ! ip.n ! ip.p
  } ;
}
