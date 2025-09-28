concrete QuestionGer of Question = CatGer ** open ResGer, Prelude in {

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
          cl = (mkClause who (agrGenNum ip.a) vp).s ! m ! t ! a ! p
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
      s = appPrep p ip.s -- todo: mit was => womit ; an was => woran  etc.
      } ;

    AdvIP ip adv = {
      s = \\c => ip.s ! c ++ adv.s ;
      a = ip.a ;
      isPron = False
      } ;

    IdetCN idet cn = 
      let 
        g = cn.g ;
        n = idet.n
      in {
        s = \\c => idet.s ! g ! c ++ cn.s ! idet.a ! n ! c ++ cn.adv ++ cn.rc ! n ++ cn.ext ;
        a = case n of {Sg => GSg g ; _ => GPl} ;
        isPron = False
      } ;

    IdetIP idet = 
      let 
        g = Neutr ; ----
        n = idet.n
      in {
        s = idet.s ! g ;
        a = case n of {Sg => GSg g ; _ => GPl} ;
        isPron = False ;
      } ;

    IdetQuant iquant num = 
      let 
        n = num.n ;
        a = iquant.a 
      in {
        s = \\g,c => let gn = gennum g n in iquant.s ! gn ! c ++ num.s ! agrAdj a gn c ;
        n = n ;
        a = a
      } ;

    AdvIAdv i a = {s = i.s ++ a.s} ;
 
    CompIAdv a = {s = \\_ => a.s ; ext = ""} ;

    CompIP ip = {s = \\_ => ip.s ! Nom ; ext = "" } ;

    -- QVP (added 9/2025, HL)
  lincat
    QVP = ResGer.VP ;

  linref
    QVP = \vp -> useInfVP False vp ;

  lin
    ComplSlashIP vps ip = -- just as ComplSlash : VPSlash -> NP -> VP
      let np = lin NP {s = table Bool {_ => ip.s} ;
                       a = agrGenNum ip.a ;
                       w = WLight ;            -- guessed
                       rc,ext = []} ;
          vp = case vps.objCtrl of { True => objAgr np vps ; _  => vps }
               ** { c2 = vps.c2 ; objCtrl = vps.objCtrl } ;
      in insertObjNP np vps.c2 vp ;

    AdvQVP vp iadv = insertAdv iadv.s vp ;
    AddAdvQVP qvp iadv = insertAdv iadv.s qvp ;

    QuestQVP ip qvp = { -- just as QuestVP, with qvp as vp
      s = \\m,t,a,p =>
        let
          who = appPrep qvp.c1 ip.s ;
          cl = (mkClause who (agrGenNum ip.a) qvp).s ! m ! t ! a ! p
        in table {
          QDir   => cl ! Main ;
          QIndir => cl ! Sub
        }
      } ;

}

