--# -coding=utf8
concrete IdiomBul of Idiom = CatBul ** open Prelude, ParadigmsBul, ResBul in {
  flags coding=utf8 ;

  flags optimize=all_subs ;

  lin
    ImpersCl vp = mkClause [] (GSg Neut) (NounP3 Pos) vp ;
    GenericCl vp = mkClause "някой" (GSg Neut) (NounP3 Pos) vp ;

    CleftNP np rs = 
      mkClause (np.s ! RSubj)
               (GSg Neut) np.p
               (insertObj (\\_ => thisRP ! np.gn ++ rs.s ! personAgr np.gn np.p) (personPol np.p) (predV verbBe)) ;        

    CleftAdv ad s = {s = \\t,a,p,o => case p of {Pos=>[]; Neg=>"не"} ++ ad.s ++ s.s } ;

    ExistNP np = ExistNPAdv np (lin Adv {s = ""}) ;
    ExistIP ip = ExistIPAdv ip (lin Adv {s = ""}) ;

    ExistNPAdv np adv = 
      { s = \\t,a,p,o => 
	          let verb = case orPol p (personPol np.p) of {
	                       Pos => mkV186 "имам" ;
	                       Neg => mkV186 "нямам" 
	                     } ;
                                 
                  agr=agrP3 (GSg Neut);

                  tenses = vpTenses (predV (singleV verb)) ! t ! a ! Pos ! agr

	          in case o of {
	               Main  => tenses ! Inv ! Perf ++ np.s ! RObj Acc ++ adv.s ;
	               Inv   => np.s ! RObj Acc ++ tenses ! Main ! Perf ++ adv.s ;
                   Quest => tenses ! Quest ! Perf ++ np.s ! RObj Acc ++ adv.s
	             }
      } ;

    ExistIPAdv ip adv = 
      mkQuestion {s = ip.s ! RSubj}
                 (mkClause "тук" ip.gn (NounP3 Pos) (insertObj (\\_ => adv.s) Pos (predV verbBe))) ;

    ProgrVP vp = {
      s   = \\_ => vp.s ! Imperf ;
      ad = vp.ad ;
      clitics = vp.clitics ;
      compl = vp.compl ;
      vtype = vp.vtype ;
      p = vp.p ;
      isSimple = False
      } ;

    ImpPl1 vp = {s = "нека" ++ daComplex Simul Pos vp ! Perf ! {gn = GPl ; p = P1}} ;
}

