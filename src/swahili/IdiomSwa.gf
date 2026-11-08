concrete IdiomSwa of Idiom = CatSwa ** open Prelude, ResSwa in {

  flags optimize=all_subs ; coding=utf8 ;
  lin
    ImpersCl vp = mkClause [] (agre G4 Sg P3) vp ;
    GenericCl vp = mkClause "mtu" (agre G1 Sg P3) vp ;

    CleftNP np rs = {s = \\_,_,b => "ni" ++ np.s ! npNom ++ rs.s ! np.a} ;

    CleftAdv ad s = {s = \\_,_,b => "ni" ++ ad.s ++ "ambapo" ++ s.s} ;

    ExistNP np = {s = \\_,_,b => case b of {Pos => "kuna" ; Neg => "hakuna"} ++ np.s ! npNom} ;

    ExistIP ip = {s = \\_,_,b,_ => ip.s ! npNom ++ case b of {Pos => "kuna" ; Neg => "hakuna"}} ;

    ExistNPAdv np adv = {s = \\_,_,b => case b of {Pos => "kuna" ; Neg => "hakuna"} ++ np.s ! npNom ++ adv.s} ;

    ExistIPAdv ip adv = {s = \\_,_,b,_ => ip.s ! npNom ++ case b of {Pos => "kuna" ; Neg => "hakuna"} ++ adv.s} ;

    ProgrVP vp = vp ;

    ImpPl1 vp = {s = "hebu" ++ vp.imp ! Pos ! Pl ++ vp.obj ! agre G1 Pl P1} ;

    ImpP3 np vp = {s = "acha" ++ np.s ! NPAcc ++ infVP Pos (agrFeatures np.a) vp} ;

    SelfAdvVP vp = insertObj (\\a => reflPron ! Ag a.g a.n a.p) vp ;
    SelfAdVVP vp = insertAdV "mwenyewe" vp ;
    SelfNP np = {
      s = \\c => np.s ! c ++ reflPron ! np.a ;
      a = np.a
      } ;
}

