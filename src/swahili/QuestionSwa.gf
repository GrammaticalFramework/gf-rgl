concrete QuestionSwa of Question = CatSwa ** open Prelude, ResSwa in {
  flags optimize=all_subs ; coding=utf8 ;
  lincat QVP = ResSwa.VP ;
  lin
    QuestCl cl = {s = \\t,a,b,q => case q of {
      QDir => "je" ++ cl.s ! t ! a ! b ; QIndir => cl.s ! t ! a ! b
      }} ;
    QuestVP ip vp = {s = \\t,a,b,_ => ip.s ! npNom ++
      vp.s ! t ! a ! b ! agre G1 ip.n P3 ++ vp.obj ! agre G1 ip.n P3} ;
    QuestSlash ip cl = {s = \\t,a,b,_ => ip.s ! npNom ++ cl.s ! t ! a ! b ++ cl.c2} ;
    QuestIAdv ia cl = {s = \\t,a,b,_ => ia.s ++ cl.s ! t ! a ! b} ;
    QuestIComp ic np = {s = \\_,_,b,_ => ic.s ++ np.s ! npNom} ;
    IdetCN d cn = {s = \\c => cn.s ! d.n ! npcase2case c ++ d.s ; n = d.n} ;
    IdetIP d = {s = \\_ => d.s ; n = d.n} ;
    AdvIP ip adv = {s = \\c => ip.s ! c ++ adv.s ; n = ip.n} ;
    IdetQuant iq num = {s = iq.s ! num.n ++ num.s ! G1 ; n = num.n} ;
    PrepIP p ip = {s = p.s ! ip.n ! G1 ++ ip.s ! npNom} ;
    AdvIAdv i a = {s = i.s ++ a.s} ;
    CompIAdv a = a ;
    CompIP ip = {s = ip.s ! npNom} ;
    ComplSlashIP vp ip = insertObj (\\_ => vp.c2 ++ ip.s ! npNom) vp ;
    AdvQVP vp ia = insertObj (\\_ => ia.s) vp ;
    AddAdvQVP vp ia = insertObj (\\_ => ia.s) vp ;
    QuestQVP ip vp = {s = \\t,a,b,_ => ip.s ! npNom ++
      vp.s ! t ! a ! b ! agre G1 ip.n P3 ++ vp.obj ! agre G1 ip.n P3} ;
}
