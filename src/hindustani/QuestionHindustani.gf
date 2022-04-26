--concrete QuestionUrd of Question = CatUrd ** open ResUrd, StringsHindustani, Prelude in {
incomplete concrete QuestionHindustani of Question = 
  CatHindustani ** open CommonHindustani, ResHindustani, Prelude in {

  flags optimize=all_subs ;
    coding = utf8;

  lin

    QuestCl cl = {
      s = \\t,p,qf => case qf of { 
	                  QDir => cl.s ! t ! p ! OQuest;
                      QIndir => agr ++ cl.s ! t! p ! ODir
					  }
				};	  

    QuestVP qp vp = 
       let cl = mkSClause [] (Ag qp.g qp.n Pers3_Near) vp;
           qp1 = qp.s ! Dir;
           qp2 = qp.s ! Obl ++ nE
          in { s = \\t,p,o => case t of {
		             VPImpPast => case vp.subj of {VIntrans => qp1 ++ cl.s ! t ! p ! ODir; _ => qp2 ++ cl.s ! t ! p ! ODir}; 
			      _         => qp1 ++ cl.s ! t ! p ! ODir
					 }
					}; 
    QuestSlash ip slash = 
     let
        -- ip1 = ip.s ! Dir;
        -- ip2 = ip.s ! Obl ++ nE
	ip1 = ip.s ! Obl ++ kw;
        ip2 = ip.s ! Obl ++ kw
     in {
      s = \\t,p,o => case t of { 
            VPImpPast => ip2 ++ slash.s ! t ! p ! ODir;
            _         => ip1 ++ slash.s ! t ! p ! ODir
            }
        };

    QuestIAdv iadv cl = { 
             s = \\t,p,_ => iadv.s ++ cl.s ! t ! p ! ODir;
                      	};

    QuestIComp icomp np = 
     let cl = mkSClause (np.s ! NPC Dir ++ icomp.s) np.a (predAux auxBe); 
	   in {
       s = \\t,p,qf => case qf of { 
	      QDir =>   cl.s ! t ! p ! ODir;
          QIndir => cl.s ! t! p ! ODir
		  }
		};

    PrepIP p ip = {s = ip.s ! Obl ++ p.s ! ip.g} ;

    AdvIP ip adv = {
      s = \\c => adv.s ! ip.g ++ ip.s ! c ;
      n = ip.n;
      g = ip.g;
      } ;
 
    IdetCN idet cn = {
      s = \\c => idet.s ! cn.g  ! c ++ cn.s ! idet.n ! c ; 
      g = cn.g;
	  n = idet.n;
      } ;

    IdetIP idet = {
     s = \\c => idet.s ! Masc ! c ; 
      n = idet.n;
	  g = Masc;
      } ;

    IdetQuant iqant num = {
      s = \\g,c => iqant.s ! num.n ! g ! c ++ num.s ; 
      n = num.n
      } ;

    CompIAdv a = a ;
    CompIP p = ss (p.s ! CommonHindustani.Dir) ;
    AdvIAdv i a = {s = i.s ++ a.s ! Masc} ;

}
