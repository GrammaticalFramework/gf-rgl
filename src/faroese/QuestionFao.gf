concrete QuestionFao of Question = CatFao ** open ResFao in {

lincat QVP = {
  s,anterior : Tense => Polarity => Str ;
  future,conditional : Polarity => Str
} ;

lin
  QuestCl cl = {
    s = cl.Interrogative ; anterior = cl.AnteriorInterrogative ;
    future = cl.FutureInterrogative ; conditional = cl.ConditionalInterrogative
  } ;
  QuestVP ip vp = {
    s = \\t,pol => ip.s ++ vp.Indicative ! t ! pol ! Masc ! persNum ip.n P3 ;
    anterior = \\t,pol => ip.s ++ perfectAux ! t ! persNum ip.n P3 ++ negStr pol ++ vp.Converb ;
    future = \\pol => ip.s ++ futureAux ! persNum ip.n P3 ++ negStr pol ++ vp.Nonfinite ;
    conditional = \\pol => ip.s ++ conditionalAux ! persNum ip.n P3 ++ negStr pol ++ vp.Nonfinite
  } ;
  QuestSlash ip cls = {
    s = \\t,pol => ip.s ++ cls.s ! t ! pol ; anterior = \\t,pol => ip.s ++ cls.s ! t ! pol ;
    future = \\pol => ip.s ++ cls.s ! Pres ! pol ; conditional = \\pol => ip.s ++ cls.s ! Past ! pol
  } ;
  QuestIAdv iadv cl = {
    s = \\t,pol => iadv.s ++ cl.Interrogative ! t ! pol ;
    anterior = \\t,pol => iadv.s ++ cl.AnteriorInterrogative ! t ! pol ;
    future = \\pol => iadv.s ++ cl.FutureInterrogative ! pol ;
    conditional = \\pol => iadv.s ++ cl.ConditionalInterrogative ! pol
  } ;
  QuestIComp icomp np = {
    s = \\t,pol => icomp.s ++ copula ! t ! persNum np.n np.p ++ negStr pol ++ np.s ! Nom ;
    anterior = \\t,pol => icomp.s ++ perfectAux ! t ! persNum np.n np.p ++ negStr pol ++ "verið" ++ np.s ! Nom ;
    future = \\pol => icomp.s ++ futureAux ! persNum np.n np.p ++ negStr pol ++ "vera" ++ np.s ! Nom ;
    conditional = \\pol => icomp.s ++ conditionalAux ! persNum np.n np.p ++ negStr pol ++ "vera" ++ np.s ! Nom
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
    s = \\t,pol => vps.Indicative ! t ! PSg P3 ++ vps.particle ++ negStr pol ++ vps.c2.s ++ ip.s ++ vps.sc ;
    anterior = \\t,pol => perfectAux ! t ! PSg P3 ++ negStr pol ++ vps.Converb ++ vps.c2.s ++ ip.s ++ vps.sc ;
    future = \\pol => futureAux ! PSg P3 ++ negStr pol ++ vps.Nonfinite ++ vps.c2.s ++ ip.s ++ vps.sc ;
    conditional = \\pol => conditionalAux ! PSg P3 ++ negStr pol ++ vps.Nonfinite ++ vps.c2.s ++ ip.s ++ vps.sc
  } ;
  AdvQVP vp iadv = {
    s = \\t,pol => vp.Indicative ! t ! pol ! Masc ! PSg P3 ++ iadv.s ;
    anterior = \\t,pol => perfectAux ! t ! PSg P3 ++ negStr pol ++ vp.Converb ++ iadv.s ;
    future = \\pol => futureAux ! PSg P3 ++ negStr pol ++ vp.Nonfinite ++ iadv.s ;
    conditional = \\pol => conditionalAux ! PSg P3 ++ negStr pol ++ vp.Nonfinite ++ iadv.s
  } ;
  AddAdvQVP qvp iadv = {
    s = \\t,pol => qvp.s ! t ! pol ++ iadv.s ;
    anterior = \\t,pol => qvp.anterior ! t ! pol ++ iadv.s ;
    future = \\pol => qvp.future ! pol ++ iadv.s ; conditional = \\pol => qvp.conditional ! pol ++ iadv.s
  } ;
  QuestQVP ip qvp = {
    s = \\t,pol => ip.s ++ qvp.s ! t ! pol ;
    anterior = \\t,pol => ip.s ++ qvp.anterior ! t ! pol ;
    future = \\pol => ip.s ++ qvp.future ! pol ; conditional = \\pol => ip.s ++ qvp.conditional ! pol
  } ;
}
