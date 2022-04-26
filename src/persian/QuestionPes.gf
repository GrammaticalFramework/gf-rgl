concrete QuestionPes of Question = CatPes ** open ResPes, Prelude in {
  flags optimize=all_subs ;
    coding = utf8;

  lin

  QuestCl cl = {
    s = \\t,p => cl.s ! t ! p ! OQuest
    };

  QuestVP qp vp =
   let cl = mkSClause [] (Ag qp.n P3) vp;
    in {s = \\t,p => qp.s ++ cl.s ! t ! p ! ODir} ;

  QuestSlash ip slash = {
    s = \\t,p => slash.subj ++ slash.c2.s ++ ip.s ++ slash.c2.ra
              ++ slash.vp ! t ! p ! ODir
    };

  QuestIAdv iadv cl = {
    s = \\t,p => iadv.s ++ cl.s ! t ! p ! ODir
    };

  QuestIComp icomp np =
   let cl = mkSClause (np2str np ++ icomp.s) np.a (predV beVerb)
    in {s = \\t,p => cl.s ! t ! p ! ODir};

    PrepIP p ip = {s = p.s ++ ip.s } ;

    AdvIP ip adv = {
      s =  ip.s ++ adv.s  ;
      n = ip.n;
      } ;

    IdetCN idet cn = {
      s = case idet.isNum of {
             False => idet.s ++ cn.s ! idet.n ! Bare ;
             True => idet.s ++ cn.s ! Sg ! Bare} ;
	  n = idet.n;
      } ;

    IdetIP idet = idet ;

    IdetQuant iqant num = {
      s = iqant.s  ++ num.s ;
      n = num.n ;
      isNum = True
      } ;

    CompIAdv a = a ;
    CompIP p = ss p.s ;
    AdvIAdv i a = {s =  a.s ++ i.s } ;


}
