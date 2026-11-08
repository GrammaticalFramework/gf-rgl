concrete QuestionTel of Question = CatTel ** open ResTel, Prelude in {
  lin
    QuestCl cl = cl ;

    QuestVP ip vp = mkClause {s = \\_ => ip.s ! Dir ; a = agrP3 Neutr ip.n} vp ;
    QuestIAdv iadv cl = cl ** {s = \\t,p => iadv.s ++ cl.s ! t ! p} ;
    QuestSlash ip slash = {
      s = \\t,p => slash.s ! t ! p ++ ip.s ! Obl ++ slash.c2.s
      } ;
    QuestIComp icomp np = mkClause np (predCopula ** {comp = \\_ => icomp.s}) ;

    PrepIP p ip = {s = ip.s ! Obl ++ p.s} ;
    AdvIP ip adv = {s = \\c => ip.s ! c ++ adv.s ; n = ip.n} ;
    AdvIAdv iadv adv = {s = adv.s ++ iadv.s} ;
    IdetCN idet cn = {s = \\c => idet.s ++ cn.s ! idet.n ! c ; n = idet.n} ;
    IdetIP idet = {s = \\_ => idet.s ; n = idet.n} ;
    IdetQuant iq num = {s = iq.s ! num.n ++ num.s ! Neutr ; n = num.n} ;
    CompIAdv adv = {s = adv.s} ;
    CompIP ip = {s = ip.s ! Dir} ;
}
