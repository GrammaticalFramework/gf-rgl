concrete QuestionSlv of Question = CatSlv ** open ResSlv,Prelude in {

------AR BEGIN
lin
  QuestVP ip vp = mkClause (ip.s ! Nom) ip.a False vp ;
  QuestCl cl = {s = \\t,a,p => "ali" ++ cl.s ! t ! a ! p} ;
  QuestSlash ip cls = {s = \\t,a,p => cls.c2.s ++ ip.s ! cls.c2.c ++ cls.s ! t ! a ! p} ;
  QuestIAdv iadv cl = {s = \\t,a,p => iadv.s ++ cl.s ! t ! a ! p} ;
  QuestIComp icomp np = mkClause icomp.s np.a np.isPron {s = copula ; s2 = \\_ => [] ; isCop = True ; refl = []} ;
  CompIAdv a = a ;
  CompIP p = ss (p.s ! Nom) ;
  AdvIAdv i a = ss (i.s ++ a.s) ;
  AdvIP ip a = ip ** {s=\\c=>ip.s!c++a.s} ;
  PrepIP prep ip = {s=prep.s++ip.s!prep.c} ;
  IdetQuant q num = {s=q.s++num.s!Masc!Nom} ;
  IdetCN det cn = {s=\\c=>det.s++cn.s!Indef!c!Pl;a={g=agender2gender cn.g;n=Pl;p=P3}} ;
  IdetIP det = {s=\\_=>det.s;a={g=Neut;n=Pl;p=P3}} ;


------AR END

}
