incomplete concrete QuestionScand of Question = 
  CatScand ** open CommonScand, ResScand, Prelude in {

  flags optimize=all_subs ;

  lin

    QuestCl cl = {
      s = \\t,a,p => 
            let cls = cl.s ! t ! a ! p 
            in table {
              QDir   => cls ! Inv ;
              QIndir => subjIf ++ cls ! Sub
              }
      } ;

    QuestVP qp vp = {
      s = \\t,a,b,q => 
        let 
          somo = case q of {
            QIndir => <"som",Sub> ;
            _      => <[],   Main>
            } ;
          cl = mkClause (qp.s ! nominative ++ somo.p1) {g = qp.g ; n = qp.n ; p = P3} vp  
        in
        cl.s ! t ! a ! b ! somo.p2
      } ;   

-- preposition stranding, default since 5/6/2016; for pied piping, see ExtraScand
    QuestSlash ip slash = {
      s = \\t,a,p => 
            let 
              cls = slash.s ! t ! a ! p ;
              who = ip.s ! accusative ;
	      agr = agrP3 ip.g ip.n ;
            in table {
              QDir   => who ++ cls ! Inv ++ slash.n3 ! agr ++ slash.c2.s ;
              QIndir => who ++ cls ! Sub ++ slash.n3 ! agr ++ slash.c2.s
              }
      } ;

    QuestIAdv iadv cl = {
      s = \\t,a,p => 
            let 
              cls = cl.s ! t ! a ! p ;
              why = iadv.s
            in table {
              QDir   => why ++ cls ! Inv ;
              QIndir => why ++ cls ! Sub
              }
      } ;

    QuestIComp icomp np = {
      s = \\t,a,p => 
            let 
              cls = 
                (mkClause (np.s ! nominative) np.a (predV verbBe)).s ! t ! a ! p ;
              why = icomp.s ! agrAdjNP np.a DIndef
            in table {
              QDir   => why ++ cls ! Inv ;
              QIndir => why ++ cls ! Sub
              }
      } ;

    PrepIP p ip = {
      s = p.s ++ ip.s ! accusative
      } ;

    AdvIP ip adv = {
      s = \\c => ip.s ! c ++ adv.s ;
      g = ip.g ;
      n = ip.n
      } ;

    IdetCN idet cn = let g = cn.g in {
      s = \\c => 
           idet.s ! g ++ cn.s ! idet.n ! idet.det ! caseNP c ;
      g = ngen2gen g ;
      n = idet.n
      } ;

    IdetIP idet = 
      let
        g = neutrum ; ---- g
      in {
        s = \\c => idet.s ! g  ;
        g = ngen2gen g ; 
        n = idet.n
        } ;

    IdetQuant idet num = {
      s = \\g => idet.s ! num.n ! g ++ num.s ! g ;
      n = num.n ;
      det = idet.det
      } ;

    AdvIAdv i a = {s = i.s ++ a.s} ;

    CompIAdv a = {s = \\_ => a.s} ;
    CompIP ip = {s = \\_ => ip.s ! nominative} ;

  lincat 
    QVP = CommonScand.VP ;
  lin
    ComplSlashIP vp np = insertObj (\\_ => vp.c2.s ++ np.s ! accusative) vp ;
    AdvQVP vp adv = insertObjPost (\\_ => adv.s) vp ;
    AddAdvQVP vp adv = insertObjPost (\\_ => adv.s) vp ;

    QuestQVP qp vp = {
      s = \\t,a,b,q => 
        let 
          somo = case q of {
            QIndir => <"som",Sub> ;
            _      => <[],   Main>
            } ;
          cl = mkClause (qp.s ! nominative ++ somo.p1) {g = qp.g ; n = qp.n ; p = P3} vp  
        in
        cl.s ! t ! a ! b ! somo.p2
      } ;   

}
