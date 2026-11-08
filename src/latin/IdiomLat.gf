concrete IdiomLat of Idiom = CatLat ** open Prelude, ResLat in {
--
--  flags optimize=all_subs ;
--
  lin
    ImpersCl vp = mkClause emptyNP vp ;
    GenericCl vp = mkClause (dummyNP "quis") vp ;
    ExistNP np = mkClause np (predV esseAux) ;
    ExistNPAdv np adv = mkClause np (insertAdv adv (predV esseAux)) ;
    ExistIP ip = {
      s=\\_=>"";o=\\_=>"";v=\\t,a,_,p=>esseAux.act ! VAct (anteriorityToVAnter a) (tenseToVTense t) ip.n P3;
      det={s,sp=\\_=>""};compl="";neg=\\_,_=>"";adv="";q=ip.s ! Nom
      } ;
    ExistIPAdv ip adv = (ExistIP ip) ** {adv=adv.s ! Posit} ;
    ProgrVP vp = vp ;
    ImpPl1 vp = {s = vp.obj ++ vp.compl ! Ag Masc Pl Nom ++ vp.imp ! VImp1 Pl ++ vp.adv} ;
    ImpP3 np vp = {s = combineNounPhrase np ! PronNonDrop ! APostN ! DPreN ! Nom ++
      vp.obj ++ vp.compl ! Ag np.g np.n Nom ++ vp.imp ! VImp2 np.n np.p ++ vp.adv} ;
    SelfAdvVP vp = vp ** {adv = vp.adv ++ "ipse"} ;
    SelfAdVVP = SelfAdvVP ;
    SelfNP np = np ** {postap={s=\\a=>case a of {Ag g n c =>
      table {Masc=>"ipse";Fem=>"ipsa";Neutr=>"ipsum"}!g}}} ;
--    ImpersCl vp = mkClause "it" (agrP3 Sg) vp ;
--    GenericCl vp = mkClause "one" (agrP3 Sg) vp ;
--
--    CleftNP np rs = mkClause "it" (agrP3 Sg) 
--      (insertObj (\\_ => rs.s ! np.a)
--        (insertObj (\\_ => combineNounPhrase np ! PronNonDrop ! rs.c) (predAux auxBe))) ;
--
--    CleftAdv ad s = mkClause "it" (agrP3 Sg) 
--      (insertObj (\\_ => conjThat ++ s.s)
--        (insertObj (\\_ => ad.s) (predAux auxBe))) ;
--
--    ExistNP np = 
--      mkClause "there" (agrP3 (fromAgr np.a).n) 
--        (insertObj (\\_ => combineNounPhrase np ! PronNonDrop ! Acc) (predAux auxBe)) ;
--
--    ExistIP ip = 
--      mkQuestion (ss (ip.s ! Nom)) 
--        (mkClause "there" (agrP3 ip.n) (predAux auxBe)) ;
--
--    ProgrVP vp = insertObj (\\a => vp.ad ++ vp.prp ++ vp.s2 ! a) (predAux auxBe) ;
--
--    ImpPl1 vp = {s = "let's" ++ infVP True vp (AgP1 Pl)} ;
--
}
