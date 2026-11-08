concrete QuestionSqi of Question = CatSqi ** open Prelude, ParamX, ResSqi, (I=IrregSqi) in {
lincat QVP = {s : Str} ;
lin
  QuestCl cl = cl ;
  QuestVP ip vp = {s=\\t,a,p=>ip.s++negation p++futureParticle t++vp.indicative!sqiTense t!agrNumber ip.a!ip.a.p!agrGender ip.a!Nom} ;
  QuestSlash ip cl = {s=\\t,a,p=>ip.s++cl.s!t!a!p++cl.c2.s} ;
  QuestIAdv i cl = {s=\\t,a,p=>i.s++cl.s!t!a!p} ;
  QuestIComp i np = {s=\\_,_,_=>i.s++(lin Verb I.jam_V).indicative!ResSqi.Pres!agrNumber np.a!np.a.p++np.s!Nom} ;
  IdetCN d cn = {s=d.s!cn.g++cn.s!Indef!Nom!d.n; a=agrgP3 cn.g d.n} ;
  IdetIP d = {s=d.s!Masc; a=agrgP3 Masc d.n} ;
  AdvIP ip a = ip ** {s=ip.s++a.s} ;
  IdetQuant q n = {s=\\g=>q.s!g++n.s; n=n.n} ;
  PrepIP p ip = {s=p.s++ip.s} ;
  AdvIAdv i a = {s=i.s++a.s} ;
  CompIAdv i = i ;
  CompIP ip = {s=ip.s} ;
  ComplSlashIP sl ip = {s=sl.indicative!ResSqi.Pres!Sg!P3++sl.c2.s++ip.s} ;
  AdvQVP vp i = {s=vp.indicative!ResSqi.Pres!Sg!P3!Masc!Nom++i.s} ;
  AddAdvQVP q i = {s=q.s++i.s} ;
  QuestQVP ip q = {s=\\_,_,_=>ip.s++q.s} ;
}
