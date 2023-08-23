concrete QuestionGer of Question = CatGer ** open ResGer in {

  flags optimize=all_subs ;

  lin

    QuestCl cl = {
      s = \\m,t,a,p => 
            let cls = cl.s ! m ! t ! a ! p 
            in table {
              QDir   => cls ! Inv ;
              QIndir => "ob" ++ cls ! Sub
              }
      } ;

    QuestVP ip vp = {
      s = \\m,t,a,p => 
        let
          who = appPrep vp.c1 ip.s ;
          cl = (mkClause who (agrP3 ip.n) vp).s ! m ! t ! a ! p
        in table {
          QDir   => cl ! Main ;
          QIndir => cl ! Sub
          }
      } ;

    QuestSlash ip slash = {
      s = \\m,t,a,p => 
            let 
              cls = slash.s ! m ! t ! a ! p ;
              who = appPrep slash.c2 ip.s ;
            in table {
              QDir   => who ++ cls ! Inv ;
              QIndir => who ++ cls ! Sub
              }
      } ;

    QuestIAdv iadv cl = {
      s = \\m,t,a,p => 
            let 
              cls = cl.s ! m ! t ! a ! p ;
              why = iadv.s
            in table {
              QDir   => why ++ cls ! Inv ;
              QIndir => why ++ cls ! Sub
              }
      } ;

    QuestIComp icomp np = {
      s = \\m,t,a,p => 
            let 
              vp  = predV sein_V ** {ext = icomp.ext};
	      subj = mkSubject np vp.c1 ;
              cls = (mkClause subj.s subj.a vp).s ! m ! t ! a ! p ;
              why = icomp.s ! np.a
            in table {
              QDir   => why ++ cls ! Inv ;
              QIndir => why ++ cls ! Sub
              }
      } ;

    PrepIP p ip = {
      s = appPrep p ip.s ;
      } ;

    AdvIP ip adv = {
      s = \\c => ip.s ! c ++ adv.s ;
      n = ip.n
      } ;

    IdetCN idet cn = 
      let 
        g = cn.g ;
        n = idet.n
      in {
      s = \\c => idet.s ! g ! c ++ cn.s ! Weak ! n ! c ; 
      n = n
      } ;

    IdetIP idet = 
      let 
        g = Neutr ; ----
        n = idet.n
      in {
      s = idet.s ! g ;
      n = n
      } ;

    IdetQuant idet num = 
      let 
        n = num.n
      in {
      s = \\g,c => idet.s ! n ! g ! c ++ num.s!g!c  ; 
      n = n
      } ;

    AdvIAdv i a = {s = i.s ++ a.s} ;
 
    CompIAdv a = {s = \\_ => a.s ; ext = ""} ;

    CompIP ip = {s = \\_ => ip.s ! Nom ; ext = "" } ;

}

